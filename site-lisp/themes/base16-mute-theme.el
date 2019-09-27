;; base16-mute-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: Some guy, years ago
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-mute-colors
  '(:base00 "#000000"
    :base01 "#b07050"
    :base02 "#50b070"
    :base03 "#a0a070"
    :base04 "#5070b0"
    :base05 "#a070a0"
    :base06 "#70a0a0"
    :base07 "#c0c2c6"
    :base08 "#505458"
    :base09 "#d09070"
    :base0A "#70d090"
    :base0B "#c0c090"
    :base0C "#7090d0"
    :base0D "#c090c0"
    :base0E "#90c0c0"
    :base0F "#ffffff")
  "All colors for Base16 Mute are defined here.")

;; Define the theme
(deftheme base16-mute)

;; Add all the faces to the theme
(base16-theme-define 'base16-mute base16-mute-colors)

;; Mark the theme as provided
(provide-theme 'base16-mute)

(provide 'base16-mute-theme)

;;; base16-mute-theme.el ends here
