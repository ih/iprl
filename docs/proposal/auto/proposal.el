(TeX-add-style-hook "proposal"
 (lambda ()
    (LaTeX-add-bibliographies
     "myBib")
    (LaTeX-add-labels
     "fig:patterns")
    (TeX-run-style-hooks
     "amsmath"
     "graphicx"
     "latex2e"
     "art12"
     "article"
     "a4paper"
     "12pt")))

