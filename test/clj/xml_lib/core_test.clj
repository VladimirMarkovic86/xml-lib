(ns xml-lib.core-test
  (:require [clojure.test :refer :all]
            [xml-lib.core :refer :all]
            [clojure.string :as cstring]))

(deftest test-find-open-tag
  
  (testing "Test find open tag"
    
    (let [string-to-parse nil
          [open-tag
           removed-open-tag] (find-open-tag
                               string-to-parse)]
      
      (is
        (nil?
          open-tag)
       )
      
      (is
        (nil?
          removed-open-tag)
       )
      
     )
    
    (let [string-to-parse " "
          [open-tag
           removed-open-tag] (find-open-tag
                               string-to-parse)]
      
      (is
        (nil?
          open-tag)
       )
      
      (is
        (nil?
          removed-open-tag)
       )
      
     )
    
    (let [string-to-parse (str
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n"
                            "</html>")
          [open-tag
           removed-open-tag] (find-open-tag
                               string-to-parse)]
      
      (is
        (= open-tag
           "html")
       )
      
      (is
        (= removed-open-tag
           (str
             "\n"
             "  <head>\n"
             "    <meta charset=\"utf-8\">\n"
             "    <title>Clojure server</title>\n"
             "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
             "  </head>\n"
             "  <body>\n"
             "    <h2>Clojure server is running</h2>\n"
             "  </body>\n"
             "</html>"))
       )
      
     )
    
    (let [string-to-parse (str
                            "\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n")
          [open-tag
           removed-open-tag] (find-open-tag
                               string-to-parse)]
      
      (is
        (= open-tag
           "head")
       )
      
      (is
        (= removed-open-tag
           (str
             "\n"
             "    <meta charset=\"utf-8\">\n"
             "    <title>Clojure server</title>\n"
             "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
             "  </head>\n"
             "  <body>\n"
             "    <h2>Clojure server is running</h2>\n"
             "  </body>\n"))
       )
      
     )
    
    (let [string-to-parse (str
                            "\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  ")
          [open-tag
           removed-open-tag] (find-open-tag
                               string-to-parse)]
      
      (is
        (= open-tag
           "meta charset=\"utf-8\"")
       )
      
      (is
        (= removed-open-tag
           (str
             "\n"
             "    <title>Clojure server</title>\n"
             "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
             "  "))
       )
      
     )
    
   )
  
 )

(deftest test-find-close-tag

  (testing "Test find close tag"
    
    (let [string-to-parse (str
                            "\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n"
                            "</html>")
          [tag-content
           after-tag] (find-close-tag
                        string-to-parse
                        nil)]
      
      (is
        (nil?
          tag-content)
       )
      
      (is
        (nil?
          after-tag)
       )
      
     )
    
    (let [string-to-parse nil
          [tag-content
           after-tag] (find-close-tag
                        string-to-parse
                        "</html>")]
      
      (is
        (nil?
          tag-content)
       )
      
      (is
        (nil?
          after-tag)
       )
      
     )
    
    (let [string-to-parse (str
                            "Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n")
          [tag-content
           after-tag] (find-close-tag
                        string-to-parse
                        "</title>")]
      
      (is
        (= tag-content
           "Clojure server")
       )
      
      (is
        (= after-tag
           "\n    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n")
       )
      
     )
    
    (let [string-to-parse (str
                            "\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n"
                            "</html>")
          [tag-content
           after-tag] (find-close-tag
                        string-to-parse
                        "</html>")]
      
      (is
        (= tag-content
           (str
             "\n"
             "  <head>\n"
             "    <meta charset=\"utf-8\">\n"
             "    <title>Clojure server</title>\n"
             "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
             "  </head>\n"
             "  <body>\n"
             "    <h2>Clojure server is running</h2>\n"
             "  </body>\n"))
       )
      
      (is
        (cstring/blank?
          after-tag)
       )
      
     )
    
    (let [string-to-parse (str
                            "\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n")
          [tag-content
           after-tag] (find-close-tag
                        string-to-parse
                        "</head>")]
      
      (is
        (= tag-content
           (str
             "\n"
             "    <meta charset=\"utf-8\">\n"
             "    <title>Clojure server</title>\n"
             "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
             "  "))
       )
      
      (is
        (= after-tag
           (str
             "\n"
             "  <body>\n"
             "    <h2>Clojure server is running</h2>\n"
             "  </body>\n"))
       )
      
     )
    
    (let [string-to-parse (str
                            "\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n")
          [tag-content
           after-tag] (find-close-tag
                        string-to-parse
                        "</meta>")]
      
      (is
        (nil?
          tag-content)
       )
      
      (is
        (= after-tag
           (str
             "\n"
             "    <title>Clojure server</title>\n"
             "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
             "  </head>\n"
             "  <body>\n"
             "    <h2>Clojure server is running</h2>\n"
             "  </body>\n"))
       )
      
     )
    
   )
  
 )

(deftest test-parse-attributes-from-string
  
  (testing "Test parse attributes from string"
    
    (let [attributes-string nil
          attributes-map (parse-attributes-from-string
                           attributes-string)]
      
      (is
        (nil?
          attributes-map)
       )
      
     )
    
    (let [attributes-string ""
          attributes-map (parse-attributes-from-string
                           attributes-string)]
      
      (is
        (nil?
          attributes-map)
       )
      
     )
    
    (let [attributes-string " charset=\"utf-8\""
          attributes-map (parse-attributes-from-string
                           attributes-string)]
      
      (is
        (= attributes-map
           {:charset "utf-8"})
       )
      
     )
    
    (let [attributes-string " charset=\"utf-8\"  class=\"content\"\n\ntestattr1=\"testattr1-value\"\t\ttestattr2=\"testattr2-value\""
          attributes-map (parse-attributes-from-string
                           attributes-string)]
      
      (is
        (= attributes-map
           {:charset "utf-8"
            :class "content"
            :testattr1 "testattr1-value"
            :testattr2 "testattr2-value"})
       )
      
     )
    
   )
  
 )

(deftest test-parse-tag-attributes
  
  (testing "Test parse tag attributes"
    
    (let [open-tag nil
          [tag-name
           attributes-map] (parse-tag-attributes
                             open-tag)]
      
      (is
        (nil?
          tag-name)
       )
      
      (is
        (nil?
          attributes-map)
       )
      
     )
    
    (let [open-tag ""
          [tag-name
           attributes-map] (parse-tag-attributes
                             open-tag)]
      
      (is
        (nil?
          tag-name)
       )
      
      (is
        (nil?
          attributes-map)
       )
      
     )
    
    (let [open-tag "meta"
          [tag-name
           attributes-map] (parse-tag-attributes
                             open-tag)]
      
      (is
        (= tag-name
           "meta")
       )
      
      (is
        (nil?
          attributes-map)
       )
      
     )
    
    (let [open-tag "meta charset=\"utf-8\""
          [tag-name
           attributes-map] (parse-tag-attributes
                             open-tag)]
      
      (is
        (= tag-name
           "meta")
       )
      
      (is
        (= attributes-map
           {:charset "utf-8"})
       )
      
     )
    
    (let [open-tag "meta charset=\"utf-8\"  class=\"content\"\n\ntestattr1=\"testattr1-value\"\t\ttestattr2=\"testattr2-value\""
          [tag-name
           attributes-map] (parse-tag-attributes
                             open-tag)]
      
      (is
        (= tag-name
           "meta")
       )
      
      (is
        (= attributes-map
           {:charset "utf-8"
            :class "content"
            :testattr1 "testattr1-value"
            :testattr2 "testattr2-value"})
       )
      
     )
    
   )
  
 )

(deftest test-html-parse-recur
  
  (testing "Test XML parse recur"
    
    (let [string-to-parse (str
                            "<!DOCTYPE html>\n"
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n"
                            "</html>")
          [parsed-to-map
           _] (html-parse-recur
                string-to-parse)]
      
      (is
        
        (= parsed-to-map
           {:tag :!DOCTYPE, :attrs {:html nil}})
        
       )
      
     )
    
    (let [string-to-parse (str
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n"
                            "</html>")
          [parsed-to-map
           _] (html-parse-recur
                string-to-parse)]
      
      (let [head-element (first
                           (:content parsed-to-map))
            title-element (get
                            (:content head-element)
                            1)
            title-element-content (:content title-element)
            title-element-content-first (first
                                          title-element-content)]
        
        (is
          (= title-element-content-first
             "Clojure server")
         )
       
       )
      
     )
    
   )
  
 )

(deftest test-html-parse-in-vector
  
  (testing "Test html parse in vector"
        
    (let [string-to-parse (str
                            "<!DOCTYPE html>\n"
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <title>Clojure server</title>\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <h2>Clojure server is running</h2>\n"
                            "  </body>\n"
                            "</html>")
          parsed-to-vector (html-parse-in-vector
                             string-to-parse)]
      
      (let [html-element (get
                           parsed-to-vector
                           1)
            head-element (first
                           (:content html-element))
            title-element (get
                            (:content head-element)
                            1)
            title-element-content (:content title-element)
            title-element-content-first (first
                                          title-element-content)]
        
        (is
          (= title-element-content-first
             "Clojure server")
         )
       
       )
      
     )
    
   )
  
 )

(deftest test-html-parse
  
  (testing "Test html parse"
    
    (let [string-to-parse (str
                            "<!DOCTYPE html>\n"
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
                            "    <title>Sample</title>\n"
                            "    <link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Lato:100,300,400\">\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"assets/stylesheet/base/normalize.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <!-- HTML -->\n"
                            "    <div></div>\n"
                            "    <!-- JS -->\n"
                            "    <div>\n"
                            "      <script type=\"text/javascript\"\n"
                            "              src=\"assets/js/main.js\">\n"
                            "      </script>\n"
                            "    </div>\n"
                            "  </body>\n"
                            "</html>")
          parsed-to-vector (html-parse
                             string-to-parse)]
      
      (let [html-element (get
                           parsed-to-vector
                           1)
            head-element (first
                           (:content html-element))
            title-element (get
                            (:content head-element)
                            2)
            title-element-content (:content title-element)
            title-element-content-first (first
                                          title-element-content)]
        
        (is
          (= title-element-content-first
             "Sample")
         )
       
       )
      
     )
    
    (let [string-to-parse (str
                            "<!DOCTYPE html>\n"
                            "<!doctype html>\n"
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
                            "    <title>Sample</title>\n"
                            "    <link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Lato:100,300,400\">\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"assets/stylesheet/base/normalize.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <!-- HTML -->\n"
                            "    <div></div>\n"
                            "    <!-- JS -->\n"
                            "    <div>\n"
                            "      <script type=\"text/javascript\"\n"
                            "              src=\"assets/js/main.js\">\n"
                            "      </script>\n"
                            "    </div>\n"
                            "  </body>\n"
                            "</html>")
          parsed-to-vector (html-parse
                             string-to-parse)]
      
      (let [html-element (get
                           parsed-to-vector
                           2)
            head-element (first
                           (:content html-element))
            title-element (get
                            (:content head-element)
                            2)
            title-element-content (:content title-element)
            title-element-content-first (first
                                          title-element-content)]
        
        (is
          (= title-element-content-first
             "Sample")
         )
       
       )
      
     )
    
    (let [string-to-parse (str
                            "<!DOCTYPE html><!doctype html>\n"
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
                            "    <title>Sample</title>\n"
                            "    <link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Lato:100,300,400\">\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"assets/stylesheet/base/normalize.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <!-- HTML -->\n"
                            "    <div></div>\n"
                            "    <!-- JS -->\n"
                            "    <div>\n"
                            "      <script type=\"text/javascript\"\n"
                            "              src=\"assets/js/main.js\">\n"
                            "      </script>\n"
                            "    </div>\n"
                            "  </body>\n"
                            "</html>")
          parsed-to-vector (html-parse
                             string-to-parse)]
      
      (let [html-element (get
                           parsed-to-vector
                           2)
            head-element (first
                           (:content html-element))
            title-element (get
                            (:content head-element)
                            2)
            title-element-content (:content title-element)
            title-element-content-first (first
                                          title-element-content)]
        
        (is
          (= title-element-content-first
             "Sample")
         )
       
       )
      
     )
    
    (let [string-to-parse (str
                            "<?xml version="1.0" encoding=\"UTF-8\" standalone=\"no\" ?>\n"
                            "<!DOCTYPE html><!doctype html>\n"
                            "<html>\n"
                            "  <head>\n"
                            "    <meta charset=\"utf-8\">\n"
                            "    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
                            "    <title>Sample</title>\n"
                            "    <link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Lato:100,300,400\">\n"
                            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"assets/stylesheet/base/normalize.css\" >\n"
                            "  </head>\n"
                            "  <body>\n"
                            "    <!-- HTML -->\n"
                            "    <div></div>\n"
                            "    <!-- JS -->\n"
                            "    <div>\n"
                            "      <script type=\"text/javascript\"\n"
                            "              src=\"assets/js/main.js\">\n"
                            "      </script>\n"
                            "    </div>\n"
                            "  </body>\n"
                            "</html>")
          parsed-to-vector (html-parse
                             string-to-parse)]
      
      (let [html-element (get
                           parsed-to-vector
                           3)
            head-element (first
                           (:content html-element))
            title-element (get
                            (:content head-element)
                            2)
            title-element-content (:content title-element)
            title-element-content-first (first
                                          title-element-content)]
        
        (is
          (= title-element-content-first
             "Sample")
         )
       
       )
      
     )
    
   )
  
 )

