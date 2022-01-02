;;; invtr.el --- INVTR Needn't Vindicate Thorough Rationalists -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos/emacs/invtr
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; INVTR or "Inventory" or else "INVTR Needn't Vindicate Thorough
;; Rationalists" is toy of an inventory management setup that handles
;; the creation of entries and the recording of data within them.  It
;; does not reinvent grep, find, dired, and friends.  Just use those.
;; Instead, it relies on a structured file format that is easy to target
;; with regular expressions.  INVTR extends USLS, which is another toy
;; program of mine.  Read the USLS documentation about the file format
;; and the general workflow (I might do a comprehensive video one day).
;;
;; The target audience of INVTR is small business owners who want to go
;; bankrupt.  Seriously though, DON'T USE THIS HIGHLY EXPERIMENTAL
;; PROGRAM.  I might turn it into a useful package at some point, either
;; on its own or by rewriting USLS, but until then DO NOT USE THIS.  I
;; am just coming up with reasons to play with Elisp.
;;
;; The URL noted above does not exist yet.  Will add it when/if I think
;; this can be remotely useful.

;;; Code:

(require 'usls)

;; TODO 2021-12-26: Make this a `defcustom'.
(defvar invtr-directory (expand-file-name "~/Documents/inventory/"))

(defun invtr--directory ()
  "Valid name format for `invtr-directory'."
  (file-name-as-directory invtr-directory))

;; TODO 2021-12-26: Make this a `defcustom'.
(defvar invtr-known-categories
  '("plastic" "metal" "wood" "glass")
  "List of user-defined categories.
Note that `invtr-new-record' can accept an arbitrary category
and that categories are inferred from existing files, all of
which are available for completion.")

(defvar invtr--title-history '())
(defvar invtr--cost-history '())
(defvar invtr--discount-history '())
(defvar invtr--productID-history '())
(defvar invtr--producer-history '())
(defvar invtr--producer-history '())
(defvar invtr--price-history '())
(defvar invtr--quantity-history '())
(defvar invtr--dimensions-history '())
(defvar invtr--weight-history '())

(defun invtr--file-name-construction (path id categories slug dimensions weight price)
  "Construct file name of `invtr-new-record'.
Catenate PATH, ID, CATEGORIES, SLUG, DIMENSIONS, WEIGHT, PRICE in
this order.  The dimensions and weight can be nil, both at once
or separately.  In that case the constructed name will omit their
corresponding fields."
  (let ((cats (usls--categories-combine categories))
        (ext usls-file-type-extension)
        (dimensions-p (and dimensions (not (string-empty-p dimensions))))
        (weight-p (and weight (not (string-empty-p weight)))))
    (cond
     ((and dimensions-p weight-p)
      (format "%s%s--%s--%s--%s--%s--%s%s"
              path id cats slug dimensions weight price ext))
     (dimensions-p
      (format "%s%s--%s--%s--%s--%s%s"
              path id cats slug dimensions price ext))
     (weight-p
      (format "%s%s--%s--%s--%s--%s%s"
              path id cats slug weight price ext))
     (t
      (format "%s%s--%s--%s--%s%s"
              path id cats slug price ext)))))

;; ;; Test for the above:
;; (let ((path "0")
;;       (id "1")
;;       (categories "2")
;;       (slug "3")
;;       (dimensions "4")
;;       (weight "5")
;;       (price "6"))
;;   (invtr--file-name-construction path id categories slug dimensions weight price))

;; NOTE 2021-12-27: Copied from my prot-common.el
(defun invtr-percentage-change (n-original n-final)
  "Find percentage change between N-ORIGINAL and N-FINAL numbers.

When the percentage is not an integer, it is rounded to 4
floating points: 16.666666666666664 => 16.667."
  (unless (numberp n-original)
    (user-error "N-ORIGINAL must satisfy numberp"))
  (unless (numberp n-final)
    (user-error "N-FINAL must satisfy numberp"))
  (let* ((difference (float (abs (- n-original n-final))))
         (n (* (/ difference n-original) 100))
         (round (floor n)))
    (if (> n round) (string-to-number (format "%0.4f" n)) round)))

;; NOTE 2021-12-27: Copied from my prot-common.el
(defun invtr-reverse-percentage (number percent change-p)
  "Determine the original value of NUMBER given PERCENT.

CHANGE-P should specify the increase or decrease.  For simplicity,
nil means decrease while non-nil stands for an increase.

NUMBER must satisfy `numberp', while PERCENT must be `natnump'."
  (unless (numberp number)
    (user-error "NUMBER must satisfy numberp"))
  (unless (natnump percent)
    (user-error "PERCENT must satisfy natnump"))
  (let* ((pc (/ (float percent) 100))
         (pc-change (if change-p (+ 1 pc) pc))
         (n (if change-p pc-change (float (- 1 pc-change)))))
    (/ number n)))

(defun invtr--new-truecost-from-discount (cost discount)
  "Find the original value of COST given DISCOUNT."
  (if (and discount (not (string-empty-p discount)))
      (format "%.2f"
              (invtr-reverse-percentage (string-to-number cost)
                                        (string-to-number discount)
                                        nil))
    ""))

;;;###autoload
(defun invtr-new-record (title categories cost discount productID producer price quantity dimensions weight)
  "Produce a new record for the inventory at `invtr-directory'.

When called interactively, prompt for TITLE, CATEGORIES, COST,
DISCOUNT, PRODUCTID, PRODUCER, PRICE, QUANTITY.  It will also ask
for DIMENSIONS and WEIGHT though those can be left as empty to be
ignored (just type RET at the prompt).

Internally, this is a variant of `usls-new-note'."
  (interactive
   (list
    (read-string "File title of inventory item: " nil 'invtr--title-history)
    (let ((usls-directory (invtr--directory)) ; We need this to infer correct categories
          (usls-known-categories invtr-known-categories))
      (usls--categories-prompt))
    (format "%.2f" (read-number "Cost of item: " nil 'invtr--cost-history))
    (format "%s%%" (read-number "Discount? (number without % sig or '0'): " nil 'invtr--discount-history))
    (read-string "Product number/code (from producer): " nil 'invtr--productID-history)
    (read-string "Producer or supplier and Invoice No. (e.g. NAME #123456): " nil 'invtr--producer-history)
    (format "%.2f" (read-number "Price we sell at: " nil 'invtr--price-history))
    (read-string "Total quantity (e.g. '50' for pieces, '2x10' for sets): " nil 'invtr--quantity-history)
    (read-string "Dimensions (e.g 200x100cm): " nil 'invtr--dimensions-history)
    (read-string "Weight (e.g 150g): " nil 'invtr--weight-history)))
  (let* ((usls-file-type-extension ".org")
         (slug (usls--sluggify title))
         (path (invtr--directory))
         (id (format-time-string usls-id))
         (date (format-time-string "%F"))
         (profit (format "%s%%" (invtr-percentage-change (string-to-number cost) (string-to-number price))))
         (filename (invtr--file-name-construction path id categories slug dimensions weight price))
         (truecost (invtr--new-truecost-from-discount cost discount))
         (dimensions-p (and dimensions (not (string-empty-p dimensions))))
         (weight-p (and weight (not (string-empty-p weight)))))
    (with-current-buffer (find-file filename)
      (insert
       (concat "#+title:      " title "\n"
               "#+date:       " date "\n"
               "#+orig_name:  " filename "\n"
               "#+orig_id:    " id "\n"
               "#+category:   " (usls--categories-capitalize categories) "\n"
               "#+cost:       " cost "\n"
               "#+discount:   " discount "\n"
               "#+truecost:   " truecost "\n"
               "#+producer:   " producer "\n"
               "#+productID:  " productID "\n"
               "#+price:      " price "\n"
               "#+profit:     " profit "\n"
               "#+quantity:   " quantity "\n")
       (cond
        ((and dimensions-p weight-p)
         (concat
          "#+dimensions: " dimensions "\n"
          "#+weight:     " weight "\n\n"))
        (dimensions-p
         (concat "#+dimensions: " dimensions "\n\n"))
        (weight-p
         (concat "#+weight:     " weight "\n"))
        (t
         "\n"))))))

;; Test for the above

;; (invtr-new-record
;;  "This is a test"
;;  '("one" "two" "three")
;;  "2.15"
;;  "30"
;;  "12345567101092"
;;  "Name #098765"
;;  "10"
;;  "40"
;;  "10x20cm"
;;  "30g")

(defvar invtr--add-acquisition-quantity-history '())
(defvar invtr--add-acquisition-invoice-history '())

(defun invtr--maybe-add-records-heading ()
  "Add a heading for records, if necessary."
  (unless (search-forward "* Records" nil t)
    (goto-char (point-max))
    (insert "\n* Records\n")))

(defun invtr--find-key-value-pair (regexp)
  "Produce a cons cell from REGEXP by searching the file."
  (goto-char (point-min))
  (re-search-forward regexp)
  (cons (match-string-no-properties 1) (match-string-no-properties 2)))

(defun invtr--make-replacement (regexp stock total &optional float-p)
  "Help `invtr-add-acquisition', `invtr-remove-stock' restock.
REGEXP is the key to search for in the file.  STOCK is the
available quantity.  TOTAL is the stock after the performed
operation.

If optional FLOAT-P the inserted number is added with two decimal
points, such as 5 => 5.00."
  (goto-char (point-min))
  (re-search-forward regexp nil t)
  (re-search-backward stock nil t)
  (if float-p
      (replace-match (format "%.2f" (string-to-number total)))
    (replace-match total))
  (invtr--maybe-add-records-heading)
  (goto-char (point-max)))

;;;###autoload
(defun invtr-add-acquisition (quantity invoice-code)
  "Add acquisition record for QUANTITY with INVOICE-CODE."
  (interactive
   (list
    (read-string "Quantity added: " nil 'invtr--add-acquisition-quantity-history)
    (read-string "New invoice code: " nil 'invtr--add-acquisition-invoice-history)))
  (let* ((regexp "^\\(#\\+quantity:\\)\s+\\([0-9a-z]+\\)$")
         (datum (invtr--find-key-value-pair regexp))
         (key (car datum))
         (stock (cdr datum))
         (total (number-to-string (+ (string-to-number stock) (string-to-number quantity)))))
    (invtr--make-replacement regexp stock total)
    (insert
     (format "#+buy:  %s (+ %s %s) => %s, Invoice: %s\n"
             ;; The discrepancy with `invtr-remove-stock' is intentional
             ;; because we may have a scenario where we grep for "new
             ;; items" based on a timestamp which would cover those
             ;; #+buy entries as well as the #+orig_id.
             (format-time-string "%Y%m%d_%H%M%S") stock quantity total invoice-code))))

(defvar invtr--remove-stock-quantity-hist '())

;;;###autoload
(defun invtr-remove-stock (quantity &optional arg)
  "Remove QUANTITY from stock.
With optional ARG, produce a receipt buffer."
  (interactive
   (list
    (read-string "Quantity sold/removed: " nil 'invtr--remove-stock-quantity-hist)
    current-prefix-arg))
  (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
         (regexp "^\\(#\\+quantity:\\)\s+\\([0-9a-z]+\\)$")
         (datum (invtr--find-key-value-pair regexp))
         (key (car datum))
         (stock (cdr datum))
         (total (number-to-string (- (string-to-number stock) (string-to-number quantity)))))
    (invtr--make-replacement regexp stock total)
    (insert
     (format "#+sell: %s      (- %s %s) => %s\n"
             (format-time-string "%F") stock quantity total)))
  (when arg
    (invtr-create-receipt quantity)))

(defun invtr--reset-cost (cost)
  "Reset file's cost entry to COST.
Helper for `invtr-reset-cost-discount'."
  (let* ((regexp "^\\(#\\+cost:\\)\s+\\([0-9a-z.,]+\\)$")
         (datum (invtr--find-key-value-pair regexp))
         (key (car datum))
         (old-cost (cdr datum)))
    (invtr--make-replacement regexp old-cost cost :float-p)
    (cons old-cost cost)))

(defun invtr--reset-discount (discount)
  "Reset file's discount entry to DISCOUNT.
Helper for `invtr-reset-cost-discount'."
  (let* ((regexp "^\\(#\\+discount:\\)\s+\\([0-9a-z.,]+\\)%$")
         (datum (invtr--find-key-value-pair regexp))
         (key (car datum))
         (old-discount (cdr datum)))
    (invtr--make-replacement regexp old-discount discount)
    (cons old-discount discount)))

(defun invtr--reset-truecost (truecost)
  "Reset file's truecost entry to TRUECOST.
Helper for `invtr-reset-cost-discount'."
  (let* ((regexp "^\\(#\\+truecost:\\)\s+\\([0-9a-z.,]+\\)$")
         (datum (invtr--find-key-value-pair regexp))
         (key (car datum))
         (old-truecost (cdr datum)))
    (invtr--make-replacement regexp old-truecost truecost :float-p)
    (cons old-truecost truecost)))

(defvar invtr--reset-cost-history '())
(defvar invtr--reset-cost-discount-history '())

;;;###autoload
(defun invtr-reset-cost-discount (cost discount)
  "Write COST and recalculate true cost given DISCOUNT."
  (interactive
   (list
    (read-string "New cost: " nil 'invtr--reset-cost-history)
    (read-string "New discount (number without %): " nil 'invtr--reset-cost-discount-history)))
  (let* ((costs (invtr--reset-cost cost))
         (old-cost (car costs))
         (new-cost (cdr costs))
         (discounts (invtr--reset-discount discount))
         (old-discount (car discounts))
         (new-discount (cdr discounts))
         (truecost (invtr--new-truecost-from-discount cost discount))
         (truecosts (invtr--reset-truecost truecost))
         (old-truecost (car truecosts))
         (new-truecost (cdr truecosts)))
    (insert
     ;; NOTE the (% X Y) is not Lisp notation.  It just makes it easier
     ;; to parse a long list of such entries with + (acquisitions), -
     ;; (sales), % (price changes).
     (format "#+calc: %s      (%% %s %s) => %s :: Old cost, discount, true cost: (%% %s %s) => %s\n"
             (format-time-string "%F") cost discount truecost
             old-cost old-discount old-truecost))))

(defvar invtr--reset-price-history '())

(defun invtr--reset-price (price)
  "Reset file's price entry to PRICE.
Helper for `invtr-reset-price'."
  (let* ((regexp "^\\(#\\+price:\\)\s+\\([0-9,.]+\\)$")
         (datum (invtr--find-key-value-pair regexp))
         (key (car datum))
         (old-price (cdr datum)))
    (cons old-price price)))

(defun invtr--reset-price-rename (price)
  "Update file's PRICE component (rename the file accordingly).
Helper for `invtr-reset-price'."
  (let* ((file (buffer-file-name))
         ;; We are hardcoding the file extension here, which is
         ;; technically wrong, though I do not mind since I am only
         ;; including prices for org-mode files.
         (name (replace-regexp-in-string "--[0-9,.]+\\.org" (format "--%s.org" price) file)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

;;;###autoload
(defun invtr-reset-price (price)
  "Change PRICE of item."
  (interactive
   (list
    (format "%.2f" (read-number "New price: " nil 'invtr--reset-price-history))))
  (let* ((price-regexp "^\\(#\\+price:\\)\s+\\([0-9,.]+\\)$")
         (datum (invtr--find-key-value-pair price-regexp))
         (key (car datum))
         (old-price (cdr datum))
         (cost-regexp "^\\(#\\+cost:\\)\s+\\([0-9,.]+\\)$")
         (datum (invtr--find-key-value-pair cost-regexp))
         (key (car datum))
         (cost (cdr datum))
         (profit-regexp "^\\(#\\+profit:\\)\s+\\([0-9,.]+\\)%$")
         (datum (invtr--find-key-value-pair profit-regexp))
         (key (car datum))
         (old-profit (cdr datum))
         (profit (format "%.2f" (invtr-percentage-change (string-to-number cost) (string-to-number price)))))
    (invtr--reset-price-rename price)
    (invtr--make-replacement price-regexp old-price price :float-p)
    (invtr--make-replacement profit-regexp old-profit profit :float-p)
    (insert
     (format "#+markup: %s    (€ %s %s%%) :: Old cost and profit: (€ %s %s%%)\n"
             (format-time-string "%F") price profit old-price old-profit))))

;; TODO 2021-12-28: The receipt's template should be a defcustom.
(defvar invtr-receipt-template-function #'invtr--single-item-receipt
  "Function that produces a template for `invtr-create-receipt'.
It should accept a QUANTITY argument, followed by a PRICE and
TITLE of the item being recorded.

The `invtr--single-item-receipt' serves as a demo.")

(defun invtr--single-item-receipt (quantity price id title)
  "Produce receipt template given QUANTITY, PRICE, ID, TITLE.
This function is called by `invtr-create-receipt'."
  (let ((receiptid (format-time-string "%H%M%S_%d%m%Y"))
        (sum (format "%.2f" (* (string-to-number price) (string-to-number quantity)))))
    (insert
     (concat
      "Definitely not real company Ltd." "\n"
      "\n\n"
      "Sales receipt   #" receiptid      "\n"
      "================================" "\n"
      "\n\n"
      id "  " title "\n"
      "                 .........  " quantity
      " x " price   "\n"
      "\n\n"
      "                            " "———" "\n"
      "Total cost                  " sum "\n"))))

;;;###autoload
(defun invtr-create-receipt (quantity)
  "Produce receipt for QUANTITY of stock."
  (interactive
   (list
    (read-string "Quantity sold: " nil 'invtr--remove-stock-quantity-hist)))
  (let* ((title (cdr (invtr--find-key-value-pair "^\\(#\\+title:\\)\s+\\(.+\\)$")))
         (id (cdr (invtr--find-key-value-pair "^\\(#\\+orig_id:\\)\s+\\([0-9_]+\\)$")))
         (price (cdr (invtr--find-key-value-pair "^\\(#\\+price:\\)\s+\\([0-9_,.]+\\)$")))
         (date (format-time-string "%F")))
    (with-current-buffer (pop-to-buffer (format "*invtr receipt for: %s on %s*" id date))
      (delete-region (point-min) (point-max))
      (funcall invtr-receipt-template-function quantity price id title))))

(defvar invtr-receipt-multi-template-function #'invtr--multi-item-receipt
  "Function to handle `invtr-create-receipt-multiple' output.
It should accept a list of strings for ENTRIES and another list
of number strings for the calculation of TOTAL-COST.  An ENTITY
to whom the receipt is for is optional.

The `invtr--multi-item-receipt' provides a concrete example.")

(defun invtr--multi-item-receipt (receiptid entries total-cost entity &optional phone address mail)
  "Prepare receipt for `invtr-create-receipt-multiple'.

RECEIPTID is the unique timestamp of the receipt.  ENTRIES is a
list of strings that holds information about the items being
sold.  The TOTAL-COST is a list of number strings that must be
converted into a lump sum.

ENTITY is a string holding the name of the person for the receipt
is for.  Optional PHONE, ADDRESS, MAIL are strings that describe
the ENTITY."
  (let* ((receipt-heading
          (cond
           ((and (and phone (not (string-empty-p phone)))
                 (and address (not (string-empty-p address)))
                 (and mail (not (string-empty-p mail))))
            (concat "Receipt   #" receiptid "\n"
                    "--------------------------" "\n"
                    "Buyer:    " entity    "\n"
                    "Phone:    " phone     "\n"
                    "Address:  " address   "\n"
                    "Mail:     " mail     "\n"))
           ((and (and phone (not (string-empty-p phone)))
                 (and address (not (string-empty-p address))))
            (concat "Receipt   #" receiptid "\n"
                    "--------------------------" "\n"
                    "Buyer:    " entity    "\n"
                    "Phone:    " phone     "\n"
                    "Address:  " address   "\n"))
           ((and phone (not (string-empty-p phone)))
            (concat "Receipt   #" receiptid "\n"
                    "--------------------------" "\n"
                    "Buyer:    " entity    "\n"
                    "Phone:    " phone     "\n"))
           (t
            (concat "Receipt   #" receiptid "\n"
                    "--------------------------" "\n"
                    "Buyer:    " entity    "\n"))))
         (receipt-heading-sep (make-string 60 ?=)))
    (insert
     (concat
      "Definitely not real company Ltd." "\n"
      "\n\n"
      receipt-heading
      receipt-heading-sep
      "\n\n\n\n"))
    (apply #'insert entries)
    (let ((lumpsum (format "%.2f" (apply #'+ total-cost))))
      (insert
       (concat
        "\n"
        "                             " "———" "\n"
        "Total cost                   " lumpsum "\n")))))

(defvar invtr--receipt-multi-items-history '())
(defvar invtr--receipt-multi-entity-history '())

(defun invtr--multi-item-receipt-client-prompt ()
  "Prompt for client with completion, when appropriate."
  (if-let ((data (invtr--collate-client-data))
           (prompt "For whom is this receipt? "))
      ;; NOTE 2021-12-29: Should we REQUIRE-MATCH in this case?
      (completing-read prompt data nil t nil 'invtr--receipt-multi-entity-history)
    (read-string prompt nil 'invtr--receipt-multi-entity-history)))

;;;###autoload
(defun invtr-create-receipt-multiple (items entity)
  "Produce receipt for ITEMS (files) from the inventory.
ENTITY is the natural or legal personal to whom the receipt is
for.  If ENTITY is nil the field will be left blank."
  (interactive
   (list
    (completing-read-multiple "Prepare receipt for items: "
                              (let* ((dir (invtr--directory))
                                     (default-directory dir))
                                (usls--directory-files dir))
                              nil t nil 'invtr--receipt-multi-items-history)
    (invtr--multi-item-receipt-client-prompt)))
  (let ((default-directory (invtr--directory))
        (receiptid (format-time-string "%H%M%S_%d%m%Y"))
        (entries)
        (total-cost))
    (dolist (file items)
      (with-current-buffer (find-file file) ; test `find-file-noselect'.
        (let* ((title (cdr (invtr--find-key-value-pair "^\\(#\\+title:\\)\s+\\(.+\\)$")))
               (id (cdr (invtr--find-key-value-pair "^\\(#\\+orig_id:\\)\s+\\([0-9_]+\\)$")))
               (price (cdr (invtr--find-key-value-pair "^\\(#\\+price:\\)\s+\\([0-9_,.]+\\)$")))
               (quantity (read-string (format "Quantity sold for %s: " id) nil 'invtr--remove-stock-quantity-hist))
               (sum (format "%.2f" (* (string-to-number price) (string-to-number quantity)))))
          (funcall #'invtr-remove-stock quantity)
          ;; TODO 2021-12-28: Maybe we should abstract this like we do
          ;; for `invtr-receipt-multi-template-function'?
          (push (format "%s  %s\n%s %s x %s\n"
                        id title
                        "                 .........  "
                        quantity price)
                entries)
          (push (string-to-number sum) total-cost))))
    ;; TODO 2021-12-29: Use find-file with the default-directory being
    ;; an `invtr-receipt-directory'.
    (with-current-buffer (pop-to-buffer (format "%s--%s" receiptid entity))
      (if-let* ((data (cdr (assoc entity invtr--client-data))))
          (let* ((phone (nth 0 data))
                 (address (nth 1 data))
                 (mail (nth 2 data))
                 (a (or address ""))
                 (m (or mail "")))
            (funcall invtr-receipt-multi-template-function receiptid entries total-cost entity phone a m))
        (funcall invtr-receipt-multi-template-function receiptid entries total-cost entity)))))

;;;; Clients

(defvar invtr-client-directory (concat (invtr--directory) "clients")
  "Subdirectory of `invtr-directory' with client records.")

(defun invtr--client-directory ()
  "Valid name format for `invtr-client-directory'."
  (file-name-as-directory invtr-client-directory))

(defun invtr--make-clients-dir ()
  (unless (file-directory-p path)
    (make-directory path t)))

(defvar invtr--client-name-history '())
(defvar invtr--client-phone-history '())
(defvar invtr--client-adress-history '())
(defvar invtr--client-email-history '())

;;;###autoload
(defun invtr-new-client (name &optional phone address email)
  "Produce a new record of a client at `invtr-client-directory'.

When called interactively, prompt for NAME, PHONE, ADDRESS,
EMAIL.

Internally, this is a variant of `invtr-new-record' and
`usls-new-note'."
  (interactive
   (list
    (read-string "Name of client: " nil 'invtr--client-name-history)
    (read-string "Phone number: " nil 'invtr--client-phone-history)
    (read-string "Adress: " nil 'invtr--client-address-history)
    (read-string "Email: " nil 'invtr--client-email-history)))
  (let* ((usls-file-type-extension ".org")
         (slug (usls--sluggify name))
         (path (invtr--client-directory))
         (filename (format "%s%s%s" path slug usls-file-type-extension)))
    (usls--make-directory (invtr--client-directory))
    (with-current-buffer (find-file filename)
      (insert
       (concat "#+name:     " name "\n"
               "#+phone:    " phone "\n"
               "#+address:  " address "\n"
               ;; I don't use "email" because Org fontifies it
               ;; differently...
               "#+mail:     " email "\n")))))

(defvar invtr--client-data nil)

(defun invtr--collate-client-data ()
  "Gather client data in `invtr--client-data'."
  (let* ((dir (invtr--client-directory))
         (default-directory dir)
         (files (usls--directory-files dir)))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (let ((name (cdr (invtr--find-key-value-pair "^\\(#\\+name:\\)\s+\\(.+\\)$")))
              (phone (cdr (invtr--find-key-value-pair "^\\(#\\+phone:\\)\s+\\(.+\\)$")))
              (address (cdr (invtr--find-key-value-pair "^\\(#\\+address:\\)\s+\\(.+\\)$")))
              (email (cdr (invtr--find-key-value-pair "^\\(#\\+mail:\\)\s+\\(.+\\)$"))))
          (push (list name phone address email) invtr--client-data)))))
  (delete-dups invtr--client-data))

;;;; Minor mode setup

(defun invtr-usls-mode-activate ()
  "Activate usls mode when inside `invtr-directory'."
  (when (or (string-match-p (abbreviate-file-name invtr-directory) default-directory)
            (string-match-p (invtr--directory) default-directory)
            (string-match-p invtr-directory default-directory))
    (usls-mode 1)))

;; FIXME 2021-12-26: This should not stay like this.
(add-hook 'dired-mode-hook #'invtr-usls-mode-activate)

(provide 'invtr)

;;; invtr.el ends here
