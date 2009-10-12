;;; -*- auto-recompile: t -*-
;;; Time-stamp: <2001-01-24 11:28:13 deego>

;;;====================================================
;;; some global keys:
;;;====================================================
(ignore-errors
  (global-set-key "\C-x  eb" 'elder-buffer))
(ignore-errors
  (global-set-key "\C-x  ed" 'elder-defaults))
(ignore-errors
  (global-set-key "\C-x  eh" 'elder-ehtml-this-file))
(ignore-errors
  (global-set-key "\C-x  eH" 'elder-ehtml))
(ignore-errors 
  (global-set-key "\C-x  el" 'elder-life))  
(ignore-errors 
  (global-set-key "\C-x  em" 'elder-emat-this-file))  ; C-x SPC SPC m
(ignore-errors 
  (global-set-key "\C-x  eM" 'elder-emat))  ; C-x SPC SPC m
(ignore-errors
  (global-set-key "\C-x  er" 'elder-require))  ; C-x SPC SPC ep
(ignore-errors
  (global-set-key "\C-x  es" 'elder-substitute-interactive))  ; C-x SPC SPC s
(ignore-errors
  (global-set-key "\C-x  ES" 'elder-substitute))  ; C-x SPC SPC s
(ignore-errors
  (global-set-key "\C-x  et" 'elder-etex-this-file))  ; C-x SPC SPC e t
(ignore-errors
  (global-set-key "\C-x  eT" 'elder-etex))  ; C-x SPC SPC e t

;;;====================================================
;;;====================================================

(provide 'elder-set-keys)


;;;elder-set-keys.el ends here..

