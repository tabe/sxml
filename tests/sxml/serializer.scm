#!r6rs

(import (rnrs (6))
        (sxml serializer)
        (xunit))

(define-syntax assert-sxml
  (syntax-rules ()
    ((_ str tree)
     (assert-string=? str (srl:sxml->xml tree)))))

(assert-sxml "<hello />"
             '(*TOP* (hello)))
(assert-sxml "<?xml encoding='UTF-8'?>\n<hello />"
             '(*TOP* (*PI* xml "encoding='UTF-8'") (hello)))

(assert-sxml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xhtml:html xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <xhtml:head>
    <xhtml:title>Virtual Library</xhtml:title>
  </xhtml:head>
  <xhtml:body>
    <xhtml:p>Moved to <xhtml:a href=\"http://example.org/\">example.org</xhtml:a>.</xhtml:p>
  </xhtml:body>
</xhtml:html>"
             '(*TOP* (^ (*NAMESPACES* (xhtml "http://www.w3.org/1999/xhtml"))) (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"") (xhtml:html (^ (xml:lang "en") (lang "en")) (xhtml:head (xhtml:title "Virtual Library")) (xhtml:body (xhtml:p "Moved to " (xhtml:a (^ (href "http://example.org/")) "example.org") ".")))))

(assert-sxml "<x:html xmlns:x=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <x:head>
    <x:title>A Math Example</x:title>
  </x:head>
  <x:body>
    <x:p>The following is MathML markup:</x:p>
    <m:math xmlns:m=\"http://www.w3.org/1998/Math/MathML\">
      <m:apply>
        <m:log />
        <m:logbase>
          <m:cn> 3 </m:cn>
        </m:logbase>
        <m:ci> x </m:ci>
      </m:apply>
    </m:math>
  </x:body>
</x:html>"
             '(*TOP* (^ (*NAMESPACES* (x "http://www.w3.org/1999/xhtml") (m "http://www.w3.org/1998/Math/MathML"))) (x:html (^ (xml:lang "en") (lang "en")) (x:head (x:title "A Math Example")) (x:body (x:p "The following is MathML markup:") (m:math (m:apply (m:log) (m:logbase (m:cn " 3 ")) (m:ci " x ")))))))

(assert-sxml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<books:book xmlns:books=\"urn:loc.gov:books\" xml:lang=\"en\" lang=\"en\">
  <books:title>Cheaper by the Dozen</books:title>
  <isbn:number xmlns:isbn=\"urn:ISBN:0-395-36341-6\">1568491379</isbn:number>
  <books:notes>
    <x:p xmlns:x=\"http://www.w3.org/1999/xhtml\">
        This is also available <x:a href=\"http://www.w3.org/\">online</x:a>.
    </x:p>
  </books:notes>
</books:book>"
             '(*TOP* (^ (*NAMESPACES* (x "http://www.w3.org/1999/xhtml") (books "urn:loc.gov:books") (isbn "urn:ISBN:0-395-36341-6"))) (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"") (books:book (^ (xml:lang "en") (lang "en")) (books:title "Cheaper by the Dozen") (isbn:number "1568491379") (books:notes (x:p "\n        This is also available " (x:a (^ (href "http://www.w3.org/")) "online") ".\n    ")))))

(report)
