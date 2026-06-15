;;; stack-the-states.el --- Generate stacks of evil states with common navigational keybinds -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package was inspired by Rigpa, but is meant to be simpler & not require as many dependencies.

;;; Code:

(require 'eieio)
(require 'evil)

(defvar stack-the-states-current-stack 'evil)
(defvar stack-the-states-out-of-bounds-hook '())
(defvar stack-the-states-stacks '())

(defclass stack-the-states-stack-manager ()
  ((default-state :initarg :default-state
		 :type function
		 :custom function
		 :documentation "The function to run when entering the layout.")
  (state-stack :initarg :state-stack
	       :documentation "A list of evil state switching functions")))

(defun stack-the-states--get-current-state ()
  "Return the current stack of evil states."
  (alist-get stack-the-states-current-stack stack-the-states-stacks))

(defun stack-the-states--default ()
  "Return to the current stack's default evil state."
  (funcall (eieio-oref (stack-the-states--get-current-state) 'default-state)))

(defun stack-the-states--get-state-stack ()
  "Return to the current stack's default evil state."
  (eieio-oref (stack-the-states--get-current-state) 'state-stack))

(defun stack-the-states--do-action (inc)
  "Switch to the next/previous state in the stack, or return to the default state if we are outside of the stack. INC is the function that increments our position in the let."
  (let ((current-state (intern (concat "evil-" (symbol-name evil-state) "-state")))
      (state-stack (stack-the-states--get-state-stack)))
  (if (not (-contains? state-stack current-state))
      (stack-the-states--default)
    (let* ((pos (seq-position state-stack current-state))
	   (newpos (funcall inc pos 1)))
      (if (or (>= newpos (length state-stack)) (< newpos 0))
	  (run-hooks 'stack-the-states-out-of-bounds-hook)
	(funcall (nth newpos state-stack)))))))

(defun stack-the-states-go-up ()
  "Go up 1 state, or return to the default state if we are outside the stack."
  (interactive)
  (stack-the-states--do-action '+))

(defun stack-the-states-go-down ()
  "Go down 1 state, or return to the default state if we are outside the stack."
  (interactive)
  (stack-the-states--do-action '-))

(defun stack-the-states-make-stack (name manager)
  "Add a stack manager to 'stack-the-states-stacks'. NAME is a symbol containing the stack's name. MANAGER is a 'stack-the-states-stack-manager'."
  (add-to-list 'stack-the-states-stacks `(,name . ,manager)))

(defun stack-the-states-switch-to-stack (stack)
  "Switch to the stack mapped to STACK."
  (setq-local stack-the-states-current-stack stack)
  (stack-the-states--default))

(defun stack-the-states-switch-stack ()
  "Switch to a different stack of states."
  (interactive)
  (stack-the-states-switch-to-stack (intern (completing-read "Select a stack: " stack-the-states-stacks))))

(stack-the-states-make-stack 'evil (stack-the-states-stack-manager :default-state 'evil-normal-state :state-stack '(evil-insert-state evil-normal-state)))
(stack-the-states-make-stack 'motion (stack-the-states-stack-manager :default-state 'evil-motion-state :state-stack '(evil-motion-state)))
(stack-the-states-make-stack 'emacs (stack-the-states-stack-manager :default-state 'evil-emacs-state :state-stack '(evil-emacs-state)))

(provide 'stack-the-states)
;;; stack-the-states.el ends here
