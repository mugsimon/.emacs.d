(when (equal timu-macos-flavour "dark")
  (let ((class '((class color) (min-colors 89)))
        (bg        (if timu-macos-muted-colors "#222327" "#222327"))
        (bg-org    (if timu-macos-muted-colors "#202125" "#202125"))
        (bg-other  (if timu-macos-muted-colors "#2a2a2a" "#2a2a2a"))
        (macos0    (if timu-macos-muted-colors "#2c2c2c" "#2c2c2c"))
        (macos1    (if timu-macos-muted-colors "#393939" "#393939"))
        (macos2    (if timu-macos-muted-colors "#616161" "#616161"))
        (macos3    (if timu-macos-muted-colors "#5e5e5e" "#5e5e5e"))
        (macos4    (if timu-macos-muted-colors "#8c8c8c" "#8c8c8c"))
        (macos5    (if timu-macos-muted-colors "#b3b3b3" "#b3b3b3"))
        (macos6    (if timu-macos-muted-colors "#b3b3b3" "#b3b3b3"))
        (macos7    (if timu-macos-muted-colors "#e8e8e8" "#e8e8e8"))
        (macos8    (if timu-macos-muted-colors "#f4f4f4" "#f4f4f4"))
        (fg        (if timu-macos-muted-colors "#ffffff" "#ffffff"))
        (fg-other  (if timu-macos-muted-colors "#dedede" "#dedede"))

        (grey      (if timu-macos-muted-colors "#d2d2d2" "#8c8c8c"))
        (red       (if timu-macos-muted-colors "#ffa596" "#ff6e64"))
        (darkred   (if timu-macos-muted-colors "#ff8478" "#cc5850"))
        (orange    (if timu-macos-muted-colors "#ffd760" "#ffb350"))
        (green     (if timu-macos-muted-colors "#9fffac" "#6ae073"))
        (blue      (if timu-macos-muted-colors "#75ecff" "#4e9dff"))
        (magenta   (if timu-macos-muted-colors "#ffb8ff" "#e45c9c"))
        (teal      (if timu-macos-muted-colors "#d7ffff" "#91f3e7"))
        (yellow    (if timu-macos-muted-colors "#ffff82" "#ffde57"))
        (darkblue  (if timu-macos-muted-colors "#7abeff" "#009dff"))
        (purple    (if timu-macos-muted-colors "#e19ae9" "#cd7bf6"))
        (cyan      (if timu-macos-muted-colors "#00ffff" "#00d1e9"))
        (lightcyan (if timu-macos-muted-colors "#ceffff" "#88c0d0"))
        (darkcyan  (if timu-macos-muted-colors "#98ddeb" "#5297a5"))

        (black     (if timu-macos-muted-colors "#000000" "#000000"))
        (white     (if timu-macos-muted-colors "#ffffff" "#ffffff"))
        ;; ms: additonal color
        (lightblue (if timu-macos-muted-colors "#7abeff" "#7abeff"))
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
