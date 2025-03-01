(when (equal timu-macos-flavour "dark")
  (let ((class '((class color) (min-colors 89)))
        (bg        "#222327")
        (bg-org    "#202125")
        (bg-other  "#2a2a2a")
        (macos0    "#2c2c2c")
        (macos1    "#393939")
        (macos2    "#616161")
        (macos3    "#5e5e5e")
        (macos4    "#8c8c8c")
        (macos5    "#b3b3b3")
        (macos6    "#b3b3b3")
        (macos7    "#e8e8e8")
        (macos8    "#f4f4f4")
        (fg        "#ffffff")
        (fg-other  "#dedede")

        (grey      "#8c8c8c")
        (red       "#ff6e64")
        (darkred   "#cc5850")
        (orange    "#ffb350")
        (green     "#6ae073")
        (blue      "#4e9dff")
        (magenta   "#e45c9c")
        (teal      "#91f3e7")
        (yellow    "#ffde57")
        (darkblue  "#009dff")
        (purple    "#cd7bf6")
        (cyan      "#00d1e9")
        (lightcyan "#88c0d0")
        (darkcyan  "#5297a5")

        (black     "#000000")
        (white     "#ffffff")
        ;; ms: additonal color
        (lightblue "#7abeff")
        )

    
    (custom-set-faces
     ;; default
     `(default ((,class (:background unspecified))))
     ;; font-lock
     `(font-lock-builtin-face ((,class (:foreground ,orange))))
     `(font-lock-type-face ((,class (:foreground ,orange))))
     `(font-lock-misc-punctuation-face ((,class (:foreground unspecified))))
     ;; highlight-symbol
     `(highlight-symbol-face
       ((,class (:background ,macos1 :weight bold))))
     )
    )
  )
