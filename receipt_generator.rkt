#lang racket/gui

(require xml)
(require srfi/19)

(define frame (new frame%
				   [label "Icewire Receipt Generator"]
				   [spacing 10]))

(define main-panel (new horizontal-panel%
						[parent frame]
						[spacing 50]))

(define settings-panel (new vertical-panel%
							[parent main-panel]
							[alignment '(left top)]
							[spacing 10]))

(define item-table-panel (new vertical-panel%
							[parent main-panel]
							[alignment '(left top)]
							[style '(border)]
							[min-width 800]))

(define document-type-choice (new choice%
				  [label "Type:"]
				  [parent settings-panel]
				  [choices (list "Receipt" "Quote" "Invoice")]))

(define num-field (new text-field%
					   [label "Number:"]
					   [parent settings-panel]
					   [init-value "100000"]))

(define date-field (new text-field%
					   [label "Date:"]
					   [parent settings-panel]
					   [init-value (date->string (current-date) "~B ~d, ~Y")]))

(define purchaser-org-field (new text-field%
					   [label "Purchaser Organization:"]
					   [parent settings-panel]
					   [style '(single vertical-label)]))

(define purchaser-name-field (new text-field%
					   [label "Purchaser Name:"]
					   [parent settings-panel]
					   [style '(single vertical-label)]))

(define purchaser-email-field (new text-field%
					   [label "Purchaser Email:"]
					   [parent settings-panel]
					   [min-width 200]
					   [style '(single vertical-label)]))

(define currency-choice (new choice%
				  [label "Currency:"]
				  [parent settings-panel]
				  [choices (list "CAD" "USD")]))

(define generate-button (new button%
							 [label "&Generate"]
							 [parent frame]
							 [callback (lambda (button event)
										 (generate-document))]))

(define generate-message (new message%
							  [label ""]
							  [parent frame]
							  [font (make-font #:size 12)]
							  [auto-resize #t]))

(define (create-item-panels i)
  (cond
   [(= 0 i) empty]
   [else
	(define tmp-panel (new horizontal-panel%
						   [parent item-table-panel]))
	(cons
	 (list
	  tmp-panel
	  (new text-field%
		   [label "Quantity:"]
		   [parent tmp-panel])
	  (new text-field%
		   [label "Part No.:"]
		   [parent tmp-panel])
	  (new text-field%
		   [label "Description:"]
		   [parent tmp-panel]
		   [min-width 400])
	  (new text-field%
		   [label "Unit price: $"]
		   [parent tmp-panel]))
	 (create-item-panels (- i 1)))]))
(define item-panels (create-item-panels 25))

(define (-items-to-html items num)
  (cond
   [(empty? items) ""]
   [(string=? (send (list-ref (car items) 1) get-value) "") ""]
   [else
	(define quantity (string->number (send (list-ref (car items) 1) get-value)))
	(define part-no (send (list-ref (car items) 2) get-value))
	(define desc (send (list-ref (car items) 3) get-value))
	(define price (string->number (send (list-ref (car items) 4) get-value)))
	(define total-price (* price quantity))
	(string-append
	 (xexpr->string
	  (quasiquote
	   (tr
		(td (unquote (number->string num)))
		(td (unquote (number->string quantity)))
		(td (unquote part-no))
		(td (unquote desc))
		(td (unquote (string-append "$" (real->decimal-string price))))
		(td (unquote (string-append "$" (real->decimal-string total-price)))))))
	 (-items-to-html (cdr items) (+ num 1)))]))
(define (items-to-html)
  (-items-to-html item-panels 1))

(define (-get-total-hst items)
  (cond
   [(empty? items) 0]
   [(string=? (send (list-ref (car items) 1) get-value) "") 0]
   [else
	(let* ([quantity (string->number (send (list-ref (car items) 1) get-value))]
		   [price (string->number (send (list-ref (car items) 4) get-value))]
		   [total-price (* price quantity)])
	  (+ (* total-price 0.13) (-get-total-hst (cdr items))))]))
(define (get-total-hst)
  (-get-total-hst item-panels))

(define (-get-total-price items)
  (cond
   [(empty? items) 0]
   [(string=? (send (list-ref (car items) 1) get-value) "") 0]
   [else
	(let* ([quantity (string->number (send (list-ref (car items) 1) get-value))]
		   [price (string->number (send (list-ref (car items) 4) get-value))]
		   [total-price (* price quantity)])
	  (+ total-price (-get-total-price (cdr items))))]))
(define (get-total-price)
  (-get-total-price item-panels))

(define (generate-document)
  (define document-type (send document-type-choice get-string-selection))
  (define num (send num-field get-value))
  (define htmlfile (string-append (string-downcase (substring document-type 0 1)) num ".html"))
  (define purchaser-org (send purchaser-org-field get-value))
  (define purchaser-name (send purchaser-name-field get-value))
  (define purchaser-email (send purchaser-email-field get-value))
  (define currency (send currency-choice get-string-selection))
  (define date (send date-field get-value))
  (define total-hst (get-total-hst))
  (define subtotal (get-total-price))
  (define hst-number "HST # 811480060 RT 0001")
  (define content
	(xexpr->string
	 (quasiquote
	  (html (head
			 (style (unquote (string-append
							  "body { font-family:Arial }"
							  "p { margin:0 }"
							  "table#main, table#main th, table#main td { border:1px solid black; border-collapse:collapse }"
							  "table#main th, table#main td { padding:0.2em 0.2em 0.2em 0.2em }"
							  "table#main tr td:nth-child(1) { text-align:right }"
							  "table#main tr td:nth-child(2) { text-align:right }"
							  "table#main tr td:nth-child(3) { text-align:left }"
							  "table#main tr td:nth-child(4) { text-align:left }"
							  "table#main tr td:nth-child(5) { text-align:right }"
							  "table#main tr td:nth-child(6) { text-align:right }"
							  )))
			 (title (unquote (string-append "Icewire Technologies " document-type))))
			(body ((style "width:680px; margin-left:auto; margin-right:auto"))
				  (hr)
				  (div ((style "float:left; font-weight:bold; font-size:40px")) "Icewire Technologies")
				  (div ((style "float:right; font-weight:bold; font-size:40px; color:grey")) (unquote document-type))
				  (div ((style "clear:both; margin-bottom:20px")) (hr))
				  (table ((style "width:100%"))
						 (tr
						  (td ((style "width:33%; text-align:left; font-style:italic; font-size:16px"))
							  (p "1560 Bayview Ave, #302")
							  (p "Toronto, Ontario")
							  (p "M4G 3B8"))
						  (td ((style "width:33%; text-align:center; vertical-align:bottom"))
							  (p ((style "font-style:italic; font-weight:bold; font-size:16px")) "make@icewire.ca")
							  (p ((style "font-style:italic; font-weight:bold; font-size:16px")) "647-478-9946")
							  (p ((style "font-weight:bold; font-size:18px")) "http://make.icewire.ca"))
						  (td ((style "width:33%; text-align:right"))
							  (p ((style "font-family:serif; font-weight:bold; font-size:30px")) (unquote (string-append (substring document-type 0 1) num)))
							  (p ((style "font-size:20px")) (unquote (string-append "Date: " date))))))
				  (div ((style "background-color:#1f497d; width:100%; height:26px; margin:10px 0 10px 0")))
				  (unquote (if (or (not (string=? purchaser-org "")) (not (string=? purchaser-name "")) (not (string=? purchaser-email "")))
							   (quasiquote
								(table ((style "width:100%"))
									   (tr
										(td ((style "width:33%; text-align:left"))
											(p ((style "font-weight:bold")) "Purchaser:")
											(p ((style "font-style:italic")) (unquote purchaser-org))
											(p ((style "font-style:italic")) (unquote purchaser-name))
											(p ((style "font-style:italic")) (unquote purchaser-email)))
										(unquote (if (string=? document-type "Quote")
													 (quasiquote (td ((style "width:33%; text-align:center")) "This quote is valid for 30 days"))
													 ""))
										(td ((style "width:33%; text-align:right"))))))
							   ""))
				  (div ((style "float:right; margin-top:100px"))
					   (unquote (string-append "Currency in " currency)))
				  (table ((id "main") (style "width:100%"))
						 (tr ((style "font-weight:bold; font-size:20px"))
							 (th ((style "width:1em; padding:0 0.5em 0 0.5em")) "Item")
							 (th ((style "width:1em; padding:0 0.5em 0 0.5em")) "Quantity")
							 (th "Part No.")
							 (th "Description")
							 (th ((style "width:1em; padding:0 1em 0 1em")) "Price")
							 (th ((style "width:1em; padding:0 1em 0 1em")) "Total"))
						 "ITEM_ROWS"
						 (tr
						  (td nbsp)
						  (td nbsp)
						  (td nbsp)
						  (td nbsp)
						  (td nbsp)
						  (td nbsp))
						 (tr
						  (td nbsp)
						  (td nbsp)
						  (td nbsp)
						  (td "Subtotal")
						  (td nbsp)
						  (td (unquote (string-append "$" (real->decimal-string subtotal)))))
						 (tr
						  (td nbsp)
						  (td nbsp)
						  (td nbsp)
						  (td "HST")
						  (td nbsp)
						  (td (unquote (string-append "$" (real->decimal-string total-hst)))))
						 (tr ((style "font-weight:bold; font-size:18px"))
							 (td nbsp)
							 (td nbsp)
							 (td nbsp)
							 (td "Total")
							 (td nbsp)
							 (td (unquote (string-append "$" (real->decimal-string (+ subtotal total-hst)))))))
				  (div (unquote hst-number))
				  (hr ((style "margin-bottom:50px")))
				  (hr))))))
  (set! content (string-replace content "ITEM_ROWS" (items-to-html) #:all? #f))
  (send generate-message set-label (string-append "Generated document:\n" (path->string (current-directory)) htmlfile "\n\nReview the document before closing this program!"))
  (display-to-file content htmlfile #:mode 'text #:exists 'replace))

(send frame show #t)
