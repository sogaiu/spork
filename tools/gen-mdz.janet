# two reasons to do this instead of using %j with buffer/format or
# string/format:
#
# 1. the order of the struct fields is nicer
# 2. pepe's name comes out prettier
(defn render-front-matter
  [fm &opt buf]
  (default buf @"")
  # start struct
  (buffer/push-string buf "{")
  # populate struct with certain fields first
  (when-let [title (get fm :title)]
    # XXX: uses the title from the original frontmatter, 
    #      not what was determined from the file name
    (buffer/push-string buf ":title " `"` title `"` "\n"))
  (when-let [author (get fm :author)]
    (buffer/push-string buf " :author " `"` author `"` "\n"))
  (when-let [license (get fm :license)]
    (buffer/push-string buf " :license " `"` license `"` "\n"))
  (buffer/push-string buf ` :template "mdzdoc/main.html"` "\n")
  # put the rest of the fields in the struct
  (def ignore (invert [:title :author :license :template]))
  (each key (sort (keys fm))
    (when (not (get ignore key))
      (buffer/push-string buf " :" key " " `"` (get fm key)  `"` "\n")))
  # if last byte is newline, drop it
  (when (= (chr "\n") (get buf (dec (length buf))))
    (buffer/popn buf 1))
  # close up struct
  (buffer/push-string buf "}\n")
  #
  buf)

(defn render-top-index
  [fm title content]
  (buffer/format (render-front-matter fm)
                 ``
                 ---

                 %s

                 ``
                 content title))

(defn render-index-page
  [fm _title content]
  (buffer/format (render-front-matter fm)
                 ``
                 ---

                 %s

                 @api-docs[../../spork]

                 ``
                 content))

(defn render
  [fm title content]
  (buffer/format (render-front-matter fm)
                 ``
                 ---

                 %s

                 @api-docs("../../spork" "%s")

                 ``
                 content title))

(defn process
  [path full-path out-dir render-fn]
  (def file-ext ".mdz")
  (when (and (= :file (os/stat full-path :mode))
             (string/has-suffix? file-ext path))
    (def title (string/slice path 0 (- (inc (length file-ext)))))
    (def raw-content (slurp full-path))
    (def m (peg/match ~(sequence (capture (to "---"))
                                 "---"
                                 (capture (to -1)))
                      raw-content))
    (when m
      (def fm (parse (get m 0)))
      (def content (string/trim (get m 1)))
      (def out-path (string out-dir "/" title ".mdz"))
      (spit out-path (render-fn fm title content)))))

(defn main
  [_]
  (when (not (and (= :file (os/stat "project.janet" :mode))
                  (= :directory (os/stat "tools" :mode))))
    (eprintf "please invoke from spork root directory")
    (os/exit 1))
  #
  (when (= :directory (os/stat "doc" :mode))
    (eprintf "doc directory exists, content would get overwritten")
    (os/exit 1))
  #
  (os/mkdir "doc")
  (os/mkdir "doc/api")
  #
  (process "index.mdz" "doc/src/index.mdz" "doc" render-top-index)
  #
  (def in-dir "doc-src/api")
  (each path (os/dir in-dir)
    (def full-path (string in-dir "/" path))
    (def out-dir "doc/api")
    (if (= path "index.mdz")
      (process path full-path out-dir render-index-page)
      (process path full-path out-dir render))))
