###
### Example on using spork/services to run a number of processes.
###

(import spork/services)
(import spork/netrepl)

# Run a REPL service that you connect to via `janet-netrepl -c`
(def env (curenv))
(defn welcome
  [&]
  (with-env env
    (def buf @"\n  Currently Running Services:\n")
    (with-dyns [*out* buf]
      (services/print-all))
    buf))
(services/add-service :netrepl |(netrepl/run-server-single nil nil env nil welcome))
(services/add-service :netrepl2 |(netrepl/run-server-single nil "9999" env nil welcome))

# The absolute simplest service, printing a message in a loop until the service is cancelled.
(services/add-service :simple |(forever (setdyn :title "Saying Hello!") (print "hello!") (flush) (ev/sleep 5)))

# Run the service in a separate thread for a new event loop
# Connect to this netrepl with `janet-netrepl -U repl2.socket -c`
(services/add-service :netrepl3 services/run-module-in-thread "spork/netrepl" "run-server" :unix "repl2.socket")

# Run subprocesses as well for more isolation
(services/add-service :python-server services/run-subprocess "python" "-m" "http.server")

# Grab the weather in a loop
(services/add-service :weather-chart services/run-module-in-thread "./weather-chart" 'loop-weather)
(services/add-service :weather-viewer services/run-subprocess "feh" "tmp/weather.png")

# Print all current services to the terminal for a super simple dashboard
(ev/go
  (with-dyns [*out* stdout]
    (forever
      (prin "\e[H\e[2J\e[H")
      (print)
      (print "Connect via `janet-netrepl -c` in a new terminal.")
      (print "Individual service logs are at <service name>.log")
      (services/print-all)
      (flush)
      (ev/sleep 0.5))))

# wait until all services completed
(services/wait)

# Connect
# Run commands in the repl to manipulate services
# > (services/print-all)
# > (services/stop-service :python-server)
# > (services/start-service :python-server)
# > (services/remove-service :netrepl2)
