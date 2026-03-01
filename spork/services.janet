###
### services.janet
###
### Module for running a number of background processes in a controlled manner.
### Similar to ev-utils, but more involved with defaults for IO
### and naming fibers for debugging purposes. Services can also implicitly
### launch sibling or child services if needed.
###

(import ./path)
(import ./ev-utils)
(import ./misc)

(defdyn *current-service* "The currently running service, if there is one")
(defdyn *current-manager* "The currently running service manager, if there is one")

###
### Service Utilities
###

(defn get-service
  "Get the current service. If not in a service, raise an error"
  []
  (def service (dyn *current-service*))
  (unless service (error "not in a service"))
  service)

(defn set-title
  "Set a textual description of the service to describe what it is doing currently"
  [title]
  (put (get-service) :title title)
  title)

###
### Service Management
###

(defn- signal-handler
  [service service-name sig msg fiber]
  (def f (get service :logfile))
  (eprintf "%s from service %s: %.4q" sig service-name msg)
  (with-dyns [*err* f]
    (debug/stacktrace fiber msg "signal: "))
  (put service :last-msg msg)
  (file/flush f))

(defn- handler-fn
  "Handler function for dealing with messages from service fibers."
  [man]
  (def super (get man :supervisor))
  (def services (get man :services))
  (def services-inverse (get man :services-inverse))
  # termiate condition allows us to distinguish between an empty manager that is waiting
  # for services to be added, vs an explicitly canceled one.
  (while (or (next services) (not (get man :terminate)))
    (def [sig fiber task-id] (ev/take super))
    (def msg (fiber/last-value fiber))
    (def service-name (or task-id (get services-inverse fiber)))
    (when-let [service (get services service-name)]
      (put service :status (fiber/status (get service :fiber)))
      (signal-handler service service-name sig msg fiber))))

(defn make-manager
  "Group a number of fibers into a single object for structured concurrency.
  Also includes utilities for running services like servers in the background."
  [&opt log-dir]
  (default log-dir (os/cwd))
  (def super (ev/chan))
  (def man @{:services @{}
             :services-inverse @{}
             :log-dir log-dir
             :supervisor super})
  (put man :handler (ev/go handler-fn man super))
  man)

(defn get-manager
  "Get the current manager. If no manager exists, create one."
  []
  (def man (dyn *current-manager*))
  (if man
    man
    (setdyn *current-manager* (make-manager))))

(defn add-service
  "Spawn a service"
  [service-name main-function & args]
  (def service-name (keyword service-name))
  (def manager (get-manager))
  (if (in (get manager :services) service-name)
    (error (string "service " service-name " already exists")))
  (def log-dir (get manager :log-dir "."))
  (def logpath (path/join log-dir (string service-name ".log")))
  (eprint "starting service " service-name " - logs at " logpath)
  (def logfile (file/open logpath :ab))
  (defn wrapper
    [service]
    (setdyn *args* [(string service-name) ;args])
    (setdyn *out* logfile)
    (setdyn *err* logfile)
    (setdyn *pretty-format* "%.5q")
    (xprintf logfile "========================\nstarted service %s - args: %q" service-name args)
    (file/flush logfile)
    (setdyn *current-service* service)
    (setdyn *current-manager* manager)
    (setdyn :task-id service-name)
    (put service :status :alive)
    (put service :env (curenv))
    (put service :started-at (os/time))
    (main-function ;args))
  (def service @{:name service-name :logfile logfile :main main-function :args args})
  (def f 
    (with-dyns []
      (ev/go wrapper service (get manager :supervisor))))
  (put service :fiber f)
  (put (get manager :services) service-name service)
  (put (get manager :services-inverse) service service-name)
  (ev/sleep 0) # one loop so that wrapper has been called
  service-name)

(defn stop-service
  "Stop a running service"
  [service-name &opt reason]
  (default reason "service stopped")
  (def service-name (keyword service-name))
  (def manager (get-manager))
  (unless (in (get manager :services) service-name)
    (error (string "service " service-name " does not exist")))
  (eprint "stopping service " service-name)
  (def services (get manager :services))
  (def serv (get services service-name))
  (def logfile (get serv :logfile))
  (def f (get serv :fiber))
  (ev/cancel f reason)
  (ev/sleep 0)
  (file/close logfile)
  nil)

(defn- stop-and-start-service
  "(Re)start a service"
  [service-name restart-after reason]
  (def service-name (keyword service-name))
  (def manager (get-manager))
  (def services (get manager :services))
  (def services-inverse (get manager :services-inverse))
  (def serv (get services service-name))
  (when-let [f (get serv :fiber)]
    (when (fiber/can-resume? f)
      (stop-service service-name reason)))
  (put services-inverse (get serv :fiber) nil)
  (put services service-name nil)
  (when restart-after
    (add-service service-name (get serv :main) ;(get serv :args))))

(defn start-service
  "Start or restart a service"
  [service-name]
  (stop-and-start-service service-name true "service stopped for restart"))

(defn remove-service
  "Remove a service"
  [service-name]
  (def service-name (keyword service-name))
  (stop-and-start-service service-name false "service stopped for removal")
  (ev/sleep 0)
  # Remove from tables
  (def manager (get-manager))
  (def services (get manager :services))
  (def s (get services service-name))
  (def services-inverse (get manager :services-inverse))
  (put services service-name nil)
  (when-let [f (get s :fiber)]
    (put services-inverse f nil))
  nil)

(defn wait
  "Once a number of services have been spawned, call `wait` to
  block the fiber until the manager is canceled. This lets a manager
  fiber behave as a service itself."
  []
  (def manager (get-manager))
  (ev-utils/wait-cancel
    (put manager :terminate true)
    (eachk s (get manager :services) (stop-service s))
    (ev/cancel (get manager :handler) "kill manager")))

###
### Definition Helpers
###

(defn run-subprocess
  ``
  Create a service entry function that runs in a subprocess.
  Example usage:

  (services/add-service :my-service services/run-subprocess "janet-netrepl" "-s")
  ``
  [prog & args]
  (def f (assert (dyn *out*)))
  (assert (= :core/file (type f)))
  (os/execute [prog ;args] :px {:out f :err f}))

(defn run-module-in-thread
  ``
  A service entry function that will run on a module's function on a new thread.
  Takes the name of a module to import and a function name, and will execute function of that module.

  Example usage:

  (services/add-service :my-service services/run-module-in-thread "spork/netrepl" 'run-server-single)
  ``
  [module-name &opt func & args]
  (default func 'main)
  (def f (assert (dyn *out*)))
  (assert (= :core/file (type f)))
  (def sp (dyn *syspath*))
  (def tid (dyn :task-id))
  (ev/do-thread
    (try
      (do
        (setdyn *err* f)
        (setdyn *out* f)
        (setdyn *syspath* sp)
        (setdyn *pretty-format* "%.5q")
        (setdyn :task-id tid)
        (def main (module/value (require module-name) (symbol func)))
        (setdyn *args* [module-name ;args])
        (main ;(dyn *args*)))
      ([err f]
       (debug/stacktrace f err "in worker thread: ")
       (file/flush f) # flush f after making error stack trace
       (propagate f err)))
    (file/flush f)))

###
### Reporting
###

(defn all-services
  "Get a list of running services"
  []
  (keys (get (get-manager) :services)))

(defn- format-time
  "Convert an integer time since epoch to readable string."
  [time]
  (unless time (break ""))
  (def {:hours hours
        :minutes minutes
        :seconds seconds
        :month month
        :month-day month-day
        :year year} (os/date time))
  (string/format "%d-%.2d-%.2d %.2d:%.2d:%.2d"
                 year (inc month) (inc month-day)
                 hours minutes seconds))

(def- service-columns [:name :title :status :last-msg :started-at])
(def- service-header-map
  {:name "Name"
   :title "Title"
   :status "Status"
   :last-msg "Last Error"
   :started-at "Started At"})
(def- service-column-map
  {:started-at (fn [timestamp _row] (format-time timestamp))})

(defn print-all
  "Print a table of all running services."
  [&opt filter-fn]
  (default filter-fn (fn [&] true))
  (def manager (get-manager))
  (def services (get manager :services))
  (def raw-rows
    (seq [service-name :in (sort (all-services))]
      (get services service-name)))
  (def rows (filter filter-fn raw-rows))
  (misc/print-table rows service-columns service-header-map service-column-map))
