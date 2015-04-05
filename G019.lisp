(in-package :user)
(compile-file "procura.lisp")
(load "procura")

(defstruct posicao
	x
	y)

;Predicados
(defun estado-objectivo? (tabuleiro)
	(let ((nQueens 0))
		(progn
			(dotimes (i (array-dimension tabuleiro 0))
				(dotimes (j (array-dimension tabuleiro 1))
					(if (equal (aref tabuleiro i j) "T")
						(progn
							(incf nQueens)
							(return)))))
			(= nQueens (array-dimension tabuleiro 0)))))

(defun ameaca? (rainha1-pos rainha2-pos)
	(or (= (posicao-x rainha1-pos) (posicao-x rainha2-pos)
		(= (posicao-y rainha1-pos) (posicao-y rainha2-pos))
		(= (- (posicao-x rainha1-pos) (posicao-y rainha1-pos)) (- (posicao-x rainha2-pos) (posicao-y rainha2-pos)))
		(= (+ (posicao-x rainha1-pos) (posicao-y rainha1-pos)) (+ (posicao-x rainha2-pos) (posicao-y rainha2-pos))))))

; (defun rainha-em-conflito? (rainha-pos tabuleiro))

;Operadores
(defun coloca-rainha (tabuleiro)
	(let* ((rainhas '())
		(sucessores '())
		(linha-a-inserir 0)
		(foundQueen nil))

	(progn
		;Procurar a linha a inserir
		(dotimes (i (array-dimension tabuleiro 0))
			(progn
				(setf foundQueen nil)
				(dotimes (j (array-dimension tabuleiro 1))
					(if (equal (aref tabuleiro i j) "T")
						(progn 
							(incf linha-a-inserir)
							(setf rainhas (cons (make-posicao :x i :y j) rainhas))
							(setf foundQueen 1)
							(return))))
				(when (not foundQueen)
					(return))))
		;Procurar a coluna a inserir

		(if rainhas
			(dotimes (j (array-dimension tabuleiro 0))
				(let ((gerouSucessor nil))
					(dolist (rainha rainhas)
						(when (not gerouSucessor)
							(if (ameaca? (make-posicao :x linha-a-inserir :y j) rainha)
								(return)
								(let ((tabuleiro-copia (copy-array tabuleiro)))
									(progn
										(setf (aref tabuleiro-copia linha-a-inserir j) "T")
										(setf sucessores (cons tabuleiro-copia sucessores))
										(setf gerouSucessor 1))))))))
			(dotimes (j (array-dimension tabuleiro 0))
				(let ((tabuleiro-copia (copy-array tabuleiro)))
					(progn
						(setf (aref tabuleiro-copia linha-a-inserir j) "T")
						(setf sucessores (cons tabuleiro-copia sucessores))))))
		sucessores)))

(defun imprime-tabuleiro (tabuleiro)
	(dotimes (i (array-dimension tabuleiro 0))
		(progn
			(format t "~%")
			(dotimes (j (array-dimension tabuleiro 1))
				(format t "~a "(aref tabuleiro i j))))))


(coloca-rainha (make-array '(5 5)))


; (defun remove-rainha (rainha-pos tabuleiro)
; 	(setf (aref tabuleiro (posicao-x rainha-pos) (posicao-y rainha-pos)) nil))

; (defun resolve-problema (tabuleiro funcao)
; 	(let ((problema (cria-problema tabuleiro)
; 		))))

; (make-array '(5 5) :initial-contents '((nil nil nil nil nil)
; 									   (nil nil nil nil nil)
; 									   (nil nil nil nil nil)
; 									   (nil nil nil nil nil)
; 									   (nil nil nil T nil)))