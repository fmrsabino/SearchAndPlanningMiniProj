(in-package :user)

(defstruct posicao
	x
	y)

(defun estado-objectivo? (tabuleiro)
	(let ((nQueens 0)
		(foundQueen nil))
	(progn
		(dotimes (i (array-dimension tabuleiro 0))
			(progn
				(setf foundQueen nil)
				(dotimes (j (array-dimension tabuleiro 1))
					(when (equal (aref tabuleiro i j) "T")
						(progn
							(setf foundQueen 1)
							(incf nQueens)
							(return))))
				(when (not foundQueen)
					(return-from estado-objectivo? nil))))
		(= nQueens (array-dimension tabuleiro 0)))))

(defun ameaca? (rainha1-pos rainha2-pos)
	(or (= (posicao-x rainha1-pos) (posicao-x rainha2-pos))
		(= (posicao-y rainha1-pos) (posicao-y rainha2-pos))
		(= (- (posicao-x rainha1-pos) (posicao-y rainha1-pos)) (- (posicao-x rainha2-pos) (posicao-y rainha2-pos)))
		(= (+ (posicao-x rainha1-pos) (posicao-y rainha1-pos)) (+ (posicao-x rainha2-pos) (posicao-y rainha2-pos)))))

;Heuristica - numero de casas livres para jogar a proxima rainha
(defun heuristica (tabuleiro)
	(let ((casas-atacadas 0)
		(rainhas '())
		(foundQueen nil)
		(startLine 0)
		(nlinhas (array-dimension tabuleiro 0)))

	(progn
		(when (estado-objectivo? tabuleiro)
			(return-from heuristica 0))

		;Procurar a linha a inserir
		(dotimes (i (array-dimension tabuleiro 0))
			(progn
				(setf foundQueen nil)
				(dotimes (j (array-dimension tabuleiro 1))
					(if (equal (aref tabuleiro i j) "T")
						(progn 
							(incf startLine)
							(setf rainhas (cons (make-posicao :x i :y j) rainhas))
							(setf foundQueen 1)
							(return))))
				(when (not foundQueen)
					(return))))
		
		;Contar o numero de casas livres a partir da linha
		(loop for i from startLine to (- nlinhas 1) do
			(dotimes (j (array-dimension tabuleiro 1))
				(dolist (rainha rainhas)
					(when (ameaca? (make-posicao :x i :y j) rainha)
						(progn
							(incf casas-atacadas)
							(return))))))
		(let ((casas-totais (* (- nlinhas startLine) (array-dimension tabuleiro 1))))
			(return-from heuristica (- casas-totais casas-atacadas))))))


;Operador
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
			(dotimes (j (array-dimension tabuleiro 1))
				(progn
					(let ((ameaca nil))
						(dolist (rainha rainhas)
							(when (ameaca? (make-posicao :x linha-a-inserir :y j) rainha)
								(progn
									(setf ameaca 1)
									(return))))
						(when (not ameaca)
							(let ((tabuleiro-copia (copy-array tabuleiro)))
								(progn
									(setf (aref tabuleiro-copia linha-a-inserir j) "T")
									(setf sucessores (cons tabuleiro-copia sucessores))))))))
			(dotimes (j (array-dimension tabuleiro 1))
				(let ((tabuleiro-copia (copy-array tabuleiro)))
					(progn
						(setf (aref tabuleiro-copia linha-a-inserir j) "T")
						(setf sucessores (cons tabuleiro-copia sucessores))))))
		sucessores)))

(defun resolve-problema (estado-inicial procura-str)
	(let* ((operadores (list #'coloca-rainha))
		(problema (cria-problema estado-inicial operadores :objectivo? #'estado-objectivo? :heuristica #'heuristica)))
	(procura problema procura-str)))

;(resolve-problema (make-array '(20 20)) 'profundidade)
