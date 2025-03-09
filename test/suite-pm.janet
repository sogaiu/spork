(use ../spork/test)
(import spork/pm)

(start-suite)

(assert true) # smoke test
(assert-docs "spork/pm")

# Copy since not exposed in boot.janet
(defn- bundle-rpath
  [path]
  (string/replace-all "\\" "/" (os/realpath path)))

(defn randdir
  "Get a random directory name"
  []
  (string "tmp_dir_" (slice (string (math/random) ".tmp") 2)))

# Test mkdir -> rmdir
(assert (os/mkdir "tempdir123.tmp"))
(sh/rm "tempdir123.tmp")

# Create a temporary directory for our janet tree
(math/seedrandom (os/cryptorand 16))
(def syspath (randdir))
(sh/rm syspath)
(assert (os/mkdir syspath))
(put root-env *syspath* (bundle-rpath syspath))
(unless (os/getenv "VERBOSE")
  (setdyn *out* @""))
(assert (empty? (bundle/list)) "initial bundle/list")
(assert (empty? (bundle/topolist)) "initial bundle/topolist")

# Check our project.janet based bundle
(pm/pm-install "file::test/pm")
(assert (= 1 (length (bundle/list))) "bundle/list after install")

(end-suite)
