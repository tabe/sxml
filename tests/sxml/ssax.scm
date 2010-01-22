#!r6rs

(import (rnrs (6))
        (sxml ssax)
        (xunit))

(define-syntax assert-sxml
  (syntax-rules ()
    ((_ tree str)
     (assert-sxml tree str '()))
    ((_ tree str ns)
     (call-with-port (open-string-input-port str)
         (lambda (port)
           (assert-equal? tree (ssax:xml->sxml port ns)))))))

(assert-sxml '(*TOP* (hello))
             "<hello />")
(assert-sxml '(*TOP* (*PI* xml "encoding='UTF-8'") (hello))
             "<?xml encoding='UTF-8'?><hello />")
(assert-sxml '(*TOP* (*PI* xml "encoding='UTF-8'") (hello))
             "<?xml encoding='UTF-8'?>\n<hello>  </hello>\n")

;; http://www.w3.org/TR/xhtml1/
(let ((xhtml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html 
     PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <head>
    <title>Virtual Library</title>
  </head>
  <body>
    <p>Moved to <a href=\"http://example.org/\">example.org</a>.</p>
  </body>
</html>"))
  (assert-sxml '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"") (http://www.w3.org/1999/xhtml:html (^ (xml:lang "en") (lang "en")) (http://www.w3.org/1999/xhtml:head (http://www.w3.org/1999/xhtml:title "Virtual Library")) (http://www.w3.org/1999/xhtml:body (http://www.w3.org/1999/xhtml:p "Moved to " (http://www.w3.org/1999/xhtml:a (^ (href "http://example.org/")) "example.org") "."))))
               xhtml)
  (assert-sxml '(*TOP* (^ (*NAMESPACES* (xhtml "http://www.w3.org/1999/xhtml"))) (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"") (xhtml:html (^ (xml:lang "en") (lang "en")) (xhtml:head (xhtml:title "Virtual Library")) (xhtml:body (xhtml:p "Moved to " (xhtml:a (^ (href "http://example.org/")) "example.org") "."))))
               xhtml
               '((xhtml . "http://www.w3.org/1999/xhtml"))))
(let ((xhtml "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <head>
    <title>A Math Example</title>
  </head>
  <body>
    <p>The following is MathML markup:</p>
    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">
      <apply> <log/>
        <logbase>
          <cn> 3 </cn>
        </logbase>
        <ci> x </ci>
      </apply>
    </math>
  </body>
</html>"))
  (assert-sxml '(*TOP* (^ (*NAMESPACES* (x "http://www.w3.org/1999/xhtml"))) (x:html (^ (xml:lang "en") (lang "en")) (x:head (x:title "A Math Example")) (x:body (x:p "The following is MathML markup:") (http://www.w3.org/1998/Math/MathML:math (http://www.w3.org/1998/Math/MathML:apply (http://www.w3.org/1998/Math/MathML:log) (http://www.w3.org/1998/Math/MathML:logbase (http://www.w3.org/1998/Math/MathML:cn " 3 ")) (http://www.w3.org/1998/Math/MathML:ci " x "))))))
               xhtml
               '((x . "http://www.w3.org/1999/xhtml")))
  (assert-sxml '(*TOP* (^ (*NAMESPACES* (x "http://www.w3.org/1999/xhtml") (m "http://www.w3.org/1998/Math/MathML"))) (x:html (^ (xml:lang "en") (lang "en")) (x:head (x:title "A Math Example")) (x:body (x:p "The following is MathML markup:") (m:math (m:apply (m:log) (m:logbase (m:cn " 3 ")) (m:ci " x "))))))
               xhtml
               '((x . "http://www.w3.org/1999/xhtml") (m . "http://www.w3.org/1998/Math/MathML"))))
(let ((xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!-- initially, the default namespace is \"books\" -->
<book xmlns='urn:loc.gov:books'
    xmlns:isbn='urn:ISBN:0-395-36341-6' xml:lang=\"en\" lang=\"en\">
  <title>Cheaper by the Dozen</title>
  <isbn:number>1568491379</isbn:number>
  <notes>
    <!-- make HTML the default namespace for a hypertext commentary -->
    <p xmlns='http://www.w3.org/1999/xhtml'>
        This is also available <a href=\"http://www.w3.org/\">online</a>.
    </p>
  </notes>
</book>"))
  (assert-sxml '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"") (urn:loc.gov:books:book (^ (xml:lang "en") (lang "en")) (urn:loc.gov:books:title "Cheaper by the Dozen") (urn:ISBN:0-395-36341-6:number "1568491379") (urn:loc.gov:books:notes (http://www.w3.org/1999/xhtml:p "\n        This is also available " (http://www.w3.org/1999/xhtml:a (^ (href "http://www.w3.org/")) "online") ".\n    "))))
               xml
               '())
  (assert-sxml '(*TOP* (^ (*NAMESPACES* (x "http://www.w3.org/1999/xhtml"))) (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"") (urn:loc.gov:books:book (^ (xml:lang "en") (lang "en")) (urn:loc.gov:books:title "Cheaper by the Dozen") (urn:ISBN:0-395-36341-6:number "1568491379") (urn:loc.gov:books:notes (x:p "\n        This is also available " (x:a (^ (href "http://www.w3.org/")) "online") ".\n    "))))
               xml
               '((x . "http://www.w3.org/1999/xhtml"))))

(report)
