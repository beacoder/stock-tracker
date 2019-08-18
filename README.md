* Track stock price in Emacs

[[http://melpa.org/#/stock-tracker][file:http://melpa.org/packages/stock-tracker-badge.svg]]
[[http://stable.melpa.org/#/stock-tracker][file:http://stable.melpa.org/packages/stock-tracker-badge.svg]]

=stock-tracer= is a simple [[https://money.163.com/stock/][Netease Stock (网易股票)]] interface for Emacs.

** Installation

Install =stock-tracker= from [[http://melpa.org/][MELPA]] with:

=M-x package-install RET stock-tracker RET=

** Usage

- =stock-tracker-start= :: Start stock-tracker and display stock information
     with buffer

** Sample configuration

#+BEGIN_SRC emacs-lisp
;; Enable Cache
(setq url-automatic-caching t)

;; Example Key binding
(global-set-key (kbd "C-c y") 'stock-tracker-search-at-point)

#+END_SRC

*Notes*: A external Chinese word segmentation tool (e.g. [[https://github.com/fxsjy/jieba][结巴分词]]) is
needed to enable Chinese word segmentation support. For more info, see
[[https://github.com/xuchunyang/chinese-word-at-point.el#prerequisite][chinese-word-at-point.el#prerequisite]].
