###
### Example on using spork/services to run a number of processes.
###

(import spork/services)
(import spork/netrepl)

# Run a REPL service that you connect to via `janet-netrepl -c`
(def env (curenv))
(services/add-service :netrepl |(netrepl/run-server-single nil nil env))

# The absolute simplest service, printing a message in a loop until the service is cancelled.
(services/add-service :simple |(forever (print "hello!") (flush) (ev/sleep 5)))

# Run the service in a separate thread for a new event loop
# Connect to this netrepl with `janet-netrepl -U repl2.socket -c`
(services/add-service :netrepl2 services/run-module-in-thread "spork/netrepl" "run-server" :unix "repl2.socket")

# Run subprocesses as well for more isolation
(services/add-service :python-server services/run-subprocess "python" "-m" "http.server")

# Grab the weather in a loop
(services/add-service :weather-chart services/run-module-in-thread "./weather-chart" 'loop-weather)
(services/add-service :weather-viewer services/run-subprocess "feh" "tmp/weather.png")

(services/print-all)

# wait until all services completed
(print "connect via janet-netrepl -c")
(services/wait)

# Connect
# Run commands in the repl to manipulate services
# > (services/print-all)
# > (services/stop-service :python-server)
# > (services/start-service :python-server)
# > (services/remove-service :netrepl2)

