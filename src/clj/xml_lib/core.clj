(ns xml-lib.core
  (:require [clojure.string :as cstring]))

(defn find-open-tag
  "Finds open tag"
  [html-string]
  (when (and html-string
             (string?
               html-string)
             (not
               (cstring/blank?
                 html-string))
         )
    (let [index-of-tag-open (cstring/index-of
                              html-string
                              "<")
          index-of-tag-end (cstring/index-of
                              html-string
                              ">")]
      (if (and index-of-tag-open
               index-of-tag-end
               (< index-of-tag-open
                  index-of-tag-end))
        (let [open-tag (.substring
                         html-string
                         (inc
                           index-of-tag-open)
                         index-of-tag-end)
              removed-open-tag (.substring
                                 html-string
                                 (inc
                                   index-of-tag-end))]
          [open-tag
           removed-open-tag])
        [nil
         html-string]))
   ))

(defn find-close-tag
  "Finds close tag"
  [html-string
   close-tag]
  (when (and html-string
             (string?
               html-string)
             (not
               (cstring/blank?
                 html-string))
             close-tag
             (string?
               close-tag)
             (not
               (cstring/blank?
                 close-tag))
         )
    (let [index-of-close-tag (cstring/index-of
                               html-string
                               close-tag)]
      (if index-of-close-tag
        (let [tag-content (.substring
                            html-string
                            0
                            index-of-close-tag)
              close-tag-length (count
                                 close-tag)
              after-tag (.substring
                          html-string
                          (+ index-of-close-tag
                             close-tag-length))]
          [tag-content
           after-tag])
        [nil
         html-string]))
   ))

(defn parse-attributes-from-string
  "Parses tag attributes from string"
  [attributes-string]
  (when (and attributes-string
             (string?
               attributes-string)
             (not
               (cstring/blank?
                 attributes-string))
         )
    (let [splited-attrs (cstring/split
                          attributes-string
                          #"(\n)|(\s)|(\t)")
          removed-blanks (filter
                           (fn [param]
                             (not
                               (cstring/blank?
                                 param))
                            )
                           splited-attrs)
          attributes-map-a (atom {})]
      (doseq [attribute-string removed-blanks]
        (let [[attribute-name
               attribute-value](cstring/split
                                 attribute-string
                                 #"=\""
                                 2)
              last-value-character (last
                                     attribute-value)
              final-attribute-value (if (= last-value-character
                                           \")
                                      (.substring
                                        attribute-value
                                        0
                                        (dec
                                          (count
                                            attribute-value))
                                       )
                                      attribute-value)]
          (swap!
            attributes-map-a
            assoc
            (keyword
              attribute-name) final-attribute-value))
       )
      @attributes-map-a))
 )

(defn parse-tag-attributes
  "Parses tag attributes from open tag"
  [open-tag]
  (when (and open-tag
             (string?
               open-tag)
             (not
               (cstring/blank?
                 open-tag))
         )
    (let [first-space-index (cstring/index-of
                              open-tag
                              " ")]
      (if first-space-index
        (let [tag-name (.substring
                         open-tag
                         0
                         first-space-index)
              attributes-string (.substring
                                  open-tag
                                  (inc
                                    first-space-index))
              attributes-map (parse-attributes-from-string
                               attributes-string)]
          [tag-name
           attributes-map])
        [open-tag
         nil]))
   ))

(defn html-parse-recur
  "Parses XML string into clojure map"
  [string-to-parse
   & [html-map-a]]
  (let [html-map-a (if (instance?
                         clojure.lang.Atom
                         html-map-a)
                     html-map-a
                     (atom {}))
        after-tag-a (atom "")]
    (when (and string-to-parse
               (string?
                 string-to-parse)
               (not
                 (cstring/blank?
                   string-to-parse))
           )
      (let [[open-tag
             removed-open-tag] (find-open-tag
                                 string-to-parse)
            open-tag (if open-tag
                       (let [[tag-name
                              parsed-attributes] (parse-tag-attributes
                                                   open-tag)]
                         (swap!
                           html-map-a
                           assoc
                           :tag (keyword
                                  tag-name)
                           :attrs parsed-attributes)
                         tag-name)
                       (do
                         (reset!
                           html-map-a
                           removed-open-tag)
                         nil))
            [tag-content
             after-tag] (when open-tag
                          (find-close-tag
                            removed-open-tag
                            (str
                              "</"
                              open-tag
                              ">"))
                         )]
        (reset!
          after-tag-a
          after-tag)
        (when tag-content
          (let [new-html-map-a (atom {})
                [parsed-tag-content
                 after-tag] (html-parse-recur
                              tag-content
                              new-html-map-a)
                 after-tag-a (atom after-tag)]
            (swap!
              html-map-a
              assoc
              :content [parsed-tag-content])
            (while (and @after-tag-a
                        (string?
                          @after-tag-a)
                        (not
                          (cstring/blank?
                            @after-tag-a))
                    )
              (let [new-html-map-a (atom {})
                    [parsed-tag-content
                     after-tag] (html-parse-recur
                                  @after-tag-a
                                  new-html-map-a)]
                (reset!
                  after-tag-a
                  after-tag)
                (swap!
                  html-map-a
                  update-in
                  [:content]
                  (fn [acc
                       elem]
                    (conj
                      acc
                      elem))
                  parsed-tag-content))
             ))
         ))
     )
    [@html-map-a
     @after-tag-a]))

(defn remove-comments
  "Removes html comments <!-- -->"
  [html-string]
  (let [html-string-a (atom
                        html-string)]
    (while (and (cstring/index-of
                  @html-string-a
                  "<!--")
                (cstring/index-of
                  @html-string-a
                  "-->"))
      (let [start-comment-index (cstring/index-of
                                  @html-string-a
                                  "<!--")
            end-comment-index (cstring/index-of
                                @html-string-a
                                "-->")
            end-comment-index (+ end-comment-index
                                 3)]
        (reset!
          html-string-a
          (str
            (.substring
              @html-string-a
              0
              start-comment-index)
            (.substring
              @html-string-a
              end-comment-index))
         ))
     )
    @html-string-a))

(defn html-parse-in-vector
  "Parse HTML in vector"
  [html-string]
  (let [html-string-a (atom
                        html-string)
        html-parsed-to-vector (atom [])]
    (while (and @html-string-a
                (string?
                  @html-string-a)
                (not
                  (cstring/blank?
                    @html-string-a))
            )
      (let [[parsed-content
             after-tag] (html-parse-recur
                          @html-string-a)]
        (swap!
          html-parsed-to-vector
          conj
          parsed-content)
        (reset!
          html-string-a
          after-tag))
     )
    @html-parsed-to-vector))

(defn html-parse
  "Parses XML string into clojure map"
  [html-string]
  (let [html-string (remove-comments
                      html-string)
        parsed-xml-vector (html-parse-in-vector
                            html-string)]
    parsed-xml-vector))

