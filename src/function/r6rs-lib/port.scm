; module.exports = `

(define (newline)
  (write-char #\\linefeed)
)

(define (display obj)
  (if (or (string? obj) (char? obj))
    (write-char obj)
    (write obj)
  )
)

; `;
