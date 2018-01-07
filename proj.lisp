#| Projeto IA:
	Antonio Tavares, 78122
	Luis Borges, 78349
	Paulo Ritto, 78929
|#

;;; definicao das configuracoes possiveis para cada peca
;;peca i 
(defconstant peca-i0 (make-array (list 4 1) :initial-element T))
(defconstant peca-i1 (make-array (list 1 4) :initial-element T))
;;peca l
(defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil))))
(defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))
(defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T))))
(defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T))))
;;peca j
(defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))
(defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))
(defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))
(defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))
;;peca o
(defconstant peca-o0 (make-array (list 2 2) :initial-element T))
;;peca s
(defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))
(defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))
;;peca z
(defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil))))
(defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T))))
;;peca t
(defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil))))
(defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))

#| TIPOS ABSTRATOS DE INFORMACAO |#

#| Tipo Node |#

(defstruct node estado valor-funcao accao-origem node-pai)

#| Tipo accao |#

(defun cria-accao (pos config)
	(return-from cria-accao (cons pos config)))
	
(defun accao-coluna (accao)
	(return-from accao-coluna (car accao)))
	
(defun accao-peca (accao)
	(return-from accao-peca (cdr accao))) 

#| Tipo tabuleiro |#

(defun cria-tabuleiro()
	(return-from cria-tabuleiro (make-array '(18 10) :initial-element nil)))
	
(defun copia-tabuleiro (tabuleiro)
	(return-from copia-tabuleiro (copy-array tabuleiro)))
	
(defun tabuleiro-preenchido-p(tabuleiro linha coluna)
	(return-from tabuleiro-preenchido-p (aref tabuleiro linha coluna)))
	
(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(let ((contador 17))
		(loop while (and (>= contador 0) (not (aref tabuleiro contador coluna)))
			do (setq contador (- contador 1)))
		(return-from tabuleiro-altura-coluna (+ contador 1))))
		
(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(let ((contador 0))
		(loop while (aref tabuleiro linha contador)
			do (progn (setq contador (+ contador 1))
			(if (= contador 10)
				(return-from tabuleiro-linha-completa-p t))))
		(return-from tabuleiro-linha-completa-p nil)))
		
(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(if (and (<= linha 17) (>= linha 0) (<= coluna 9) (>= coluna 0))
		(setf (aref tabuleiro linha coluna) t)))
			 
(defun tabuleiro-remove-linha! (tabuleiro linha)
	(let ((coluna 0))
		(loop while (< linha 17) do
			(progn (loop while (<= coluna 9) do (progn (setf (aref tabuleiro linha coluna) (aref tabuleiro (1+ linha) coluna)) (incf coluna))) (incf linha) (setf coluna 0)))
			
		(loop while (<= coluna 9) do (progn (setf (aref tabuleiro linha coluna) nil) (incf coluna)))))
	
(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(let ((contador 0))
		(loop while (not (aref tabuleiro 17 contador))
			do (progn (setq contador (+ contador 1))
			(if (= contador 10)
				(return-from tabuleiro-topo-preenchido-p nil))))
		(return-from tabuleiro-topo-preenchido-p t)))
	
(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
	(return-from tabuleiros-iguais-p (equalp tabuleiro1 tabuleiro2)))
	
(defun tabuleiro->array (tabuleiro)
	(return-from tabuleiro->array (copy-array tabuleiro)))
	
(defun array->tabuleiro (array)
	(return-from array->tabuleiro (copy-array array)))


#| Tipo estado |#

(defstruct estado pontos pecas-por-colocar pecas-colocadas Tabuleiro)

(defun copia-estado (state)
	(let* ((points (estado-pontos state)) (ppc (copy-list (estado-pecas-por-colocar state))) (pc (copy-list (estado-pecas-colocadas state))) (tab (copia-tabuleiro (estado-Tabuleiro state))) (estado-novo (make-estado :pontos points :pecas-por-colocar ppc :pecas-colocadas pc :Tabuleiro tab )))
	(return-from copia-estado estado-novo)))
	
(defun estados-iguais-p (state1 state2)
	(return-from estados-iguais-p (equalp state1 state2)))
	
(defun estado-final-p (state)
	(return-from estado-final-p (or (tabuleiro-topo-preenchido-p (estado-Tabuleiro state)) (zerop (length(estado-pecas-por-colocar state))))))
	
#| Tipo problema |#

(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

(defun solucao (state)
	(return-from solucao (and (not (tabuleiro-topo-preenchido-p (estado-Tabuleiro state))) (zerop (length(estado-pecas-por-colocar state))))))
	
(defun accoes (state)
	(let ((peca (car (estado-pecas-por-colocar state))))
		(cond ((estado-final-p state) (return-from accoes nil))
			  ((equal peca 'i) 
				(return-from accoes (list (cria-accao 0 peca-i0) (cria-accao 1 peca-i0) (cria-accao 2 peca-i0) (cria-accao 3 peca-i0) (cria-accao 4 peca-i0) (cria-accao 5 peca-i0) (cria-accao 6 peca-i0) (cria-accao 7 peca-i0) (cria-accao 8 peca-i0) (cria-accao 9 peca-i0)
				        (cria-accao 0 peca-i1) (cria-accao 1 peca-i1) (cria-accao 2 peca-i1) (cria-accao 3 peca-i1) (cria-accao 4 peca-i1) (cria-accao 5 peca-i1) (cria-accao 6 peca-i1)))) 
			  ((equal peca 'l)
				(return-from accoes (list (cria-accao 0 peca-l0) (cria-accao 1 peca-l0) (cria-accao 2 peca-l0) (cria-accao 3 peca-l0) (cria-accao 4 peca-l0) (cria-accao 5 peca-l0) (cria-accao 6 peca-l0) (cria-accao 7 peca-l0) (cria-accao 8 peca-l0)
				        (cria-accao 0 peca-l1) (cria-accao 1 peca-l1) (cria-accao 2 peca-l1) (cria-accao 3 peca-l1) (cria-accao 4 peca-l1) (cria-accao 5 peca-l1) (cria-accao 6 peca-l1) (cria-accao 7 peca-l1)
				        (cria-accao 0 peca-l2) (cria-accao 1 peca-l2) (cria-accao 2 peca-l2) (cria-accao 3 peca-l2) (cria-accao 4 peca-l2) (cria-accao 5 peca-l2) (cria-accao 6 peca-l2) (cria-accao 7 peca-l2) (cria-accao 8 peca-l2)
				        (cria-accao 0 peca-l3) (cria-accao 1 peca-l3) (cria-accao 2 peca-l3) (cria-accao 3 peca-l3) (cria-accao 4 peca-l3) (cria-accao 5 peca-l3) (cria-accao 6 peca-l3) (cria-accao 7 peca-l3))))
			  ((equal peca 'j)
				(return-from accoes (list (cria-accao 0 peca-j0) (cria-accao 1 peca-j0) (cria-accao 2 peca-j0) (cria-accao 3 peca-j0) (cria-accao 4 peca-j0) (cria-accao 5 peca-j0) (cria-accao 6 peca-j0) (cria-accao 7 peca-j0) (cria-accao 8 peca-j0)
				        (cria-accao 0 peca-j1) (cria-accao 1 peca-j1) (cria-accao 2 peca-j1) (cria-accao 3 peca-j1) (cria-accao 4 peca-j1) (cria-accao 5 peca-j1) (cria-accao 6 peca-j1) (cria-accao 7 peca-j1)
				        (cria-accao 0 peca-j2) (cria-accao 1 peca-j2) (cria-accao 2 peca-j2) (cria-accao 3 peca-j2) (cria-accao 4 peca-j2) (cria-accao 5 peca-j2) (cria-accao 6 peca-j2) (cria-accao 7 peca-j2) (cria-accao 8 peca-j2)
				        (cria-accao 0 peca-j3) (cria-accao 1 peca-j3) (cria-accao 2 peca-j3) (cria-accao 3 peca-j3) (cria-accao 4 peca-j3) (cria-accao 5 peca-j3) (cria-accao 6 peca-j3) (cria-accao 7 peca-j3))))
			  ((equal peca 'o)
				(return-from accoes (list (cria-accao 0 peca-o0) (cria-accao 1 peca-o0) (cria-accao 2 peca-o0) (cria-accao 3 peca-o0) (cria-accao 4 peca-o0) (cria-accao 5 peca-o0) (cria-accao 6 peca-o0) (cria-accao 7 peca-o0) (cria-accao 8 peca-o0))))
			  ((equal peca 's)
				(return-from accoes (list (cria-accao 0 peca-s0) (cria-accao 1 peca-s0) (cria-accao 2 peca-s0) (cria-accao 3 peca-s0) (cria-accao 4 peca-s0) (cria-accao 5 peca-s0) (cria-accao 6 peca-s0) (cria-accao 7 peca-s0)
						(cria-accao 0 peca-s1) (cria-accao 1 peca-s1) (cria-accao 2 peca-s1) (cria-accao 3 peca-s1) (cria-accao 4 peca-s1) (cria-accao 5 peca-s1) (cria-accao 6 peca-s1) (cria-accao 7 peca-s1) (cria-accao 8 peca-s1))))
			  ((equal peca 'z)
				(return-from accoes (list (cria-accao 0 peca-z0) (cria-accao 1 peca-z0) (cria-accao 2 peca-z0) (cria-accao 3 peca-z0) (cria-accao 4 peca-z0) (cria-accao 5 peca-z0) (cria-accao 6 peca-z0) (cria-accao 7 peca-z0)
					 (cria-accao 0 peca-z1) (cria-accao 1 peca-z1) (cria-accao 2 peca-z1) (cria-accao 3 peca-z1) (cria-accao 4 peca-z1) (cria-accao 5 peca-z1) (cria-accao 6 peca-z1) (cria-accao 7 peca-z1) (cria-accao 8 peca-z1))))
			  ((equal peca 't)
				(return-from accoes (list (cria-accao 0 peca-t0) (cria-accao 1 peca-t0) (cria-accao 2 peca-t0) (cria-accao 3 peca-t0) (cria-accao 4 peca-t0) (cria-accao 5 peca-t0) (cria-accao 6 peca-t0) (cria-accao 7 peca-t0)
						 (cria-accao 0 peca-t1) (cria-accao 1 peca-t1) (cria-accao 2 peca-t1) (cria-accao 3 peca-t1) (cria-accao 4 peca-t1) (cria-accao 5 peca-t1) (cria-accao 6 peca-t1) (cria-accao 7 peca-t1) (cria-accao 8 peca-t1)
						 (cria-accao 0 peca-t2) (cria-accao 1 peca-t2) (cria-accao 2 peca-t2) (cria-accao 3 peca-t2) (cria-accao 4 peca-t2) (cria-accao 5 peca-t2) (cria-accao 6 peca-t2) (cria-accao 7 peca-t2)
					     (cria-accao 0 peca-t3) (cria-accao 1 peca-t3) (cria-accao 2 peca-t3) (cria-accao 3 peca-t3) (cria-accao 4 peca-t3) (cria-accao 5 peca-t3) (cria-accao 6 peca-t3) (cria-accao 7 peca-t3) (cria-accao 8 peca-t3)))))
			  (return-from accoes t))) 
					     
(defun resultado (state accao)	
	(let* ( (altura 0) (points (estado-pontos state)) (coluna (car accao)) (ppc (copy-list (estado-pecas-por-colocar state))) (pc (copy-list (estado-pecas-colocadas state))) (pc (cons (car ppc) pc)) (ppc (cdr ppc)) (tab (estado-Tabuleiro state)) 
			(state1 (make-estado :pontos points :pecas-por-colocar ppc :pecas-colocadas pc :Tabuleiro tab )) (nColunas (array-dimension (cdr accao) 1)))
		(loop for i from 0 to (1- nColunas) do (progn (if (> (1- (tabuleiro-altura-coluna tab (+ coluna i))) altura) (setq altura (1- (tabuleiro-altura-coluna tab (+ coluna i)))))))
		(loop for linha from (max 0 altura ) to 17
			do (if (pode-colocar-peca state1 accao linha)
				(progn (coloca-peca state1 accao linha) (return))))
		(if (tabuleiro-topo-preenchido-p (estado-Tabuleiro state1)) (return-from resultado state1) (remove-e-calcula state1) )
		(return-from resultado state1))) 
							
(defun qualidade(state)
	(return-from qualidade (* -1 (estado-pontos state))))
	
(defun custo-oportunidade (state)
	(let ((soma 0))
		(loop for nPeca from 0 to (1- (length (estado-pecas-colocadas state)))
			do (setf soma (+ soma (max-pontuacao-peca (nth nPeca (estado-pecas-colocadas state))))))
		(return-from custo-oportunidade (- soma (estado-pontos state)))))
		
#| Funcoes procura |#

(defun procura-generica (problema funcao)
	(let* ((fronteira (list (make-node :estado (problema-estado-inicial problema) :valor-funcao (funcall funcao (problema-estado-inicial problema)) :accao-origem nil :node-pai nil))) 
			(primeiro_no (pop fronteira)))
		(loop while (not (funcall (problema-solucao problema) (node-estado primeiro_no))) do (progn
		  (setq fronteira (expande-e-insere problema fronteira primeiro_no funcao))
		  (setq primeiro_no (pop fronteira))
		  (if (equal primeiro_no nil) (return-from procura-generica nil))))
	(return-from procura-generica (constroi-accoes primeiro_no))))
		  
(defun procura-pp (problema)
		(return-from procura-pp (procura-generica problema #'dfs-sem-heuristica)))
	
(defun procura-A* (problema heuristica)
	(return-from procura-A* (procura-generica problema (funcao-a problema heuristica))))
	
(defun procura-best (tabuleiro pecas)
	(let* ((state (make-estado :pontos 0 :pecas-colocadas () :tabuleiro tabuleiro :pecas-por-colocar pecas))
		   (problema (make-problema :estado-inicial state :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade)))
	(return-from procura-best (procura-A* problema #'heuristica-op))))
	
	
#| Funcoes auxiliares |#

;;; expande-e-insere: problema x lista x no x funcao --> lista
;;; funcao que recebe um problema, uma fronteira, um no e uma funcao e expande o no recebido,
;;; criando novos nos que correspondem a aplicar todas as accoes possiveis a esse no,
;;; calcula o novo valor de f para os novos nos e adiciona-os a fronteira, que no fim devolve.
(defun expande-e-insere (problema fronteira node funcao)
	(let* ((state (node-estado node)) (accoes-possiveis (funcall (problema-accoes problema) state)) (res) (f-value) (no))
		(loop for i from 0 to (1- (length accoes-possiveis))
			do (progn (setq res (funcall (problema-resultado problema) state (nth i accoes-possiveis)))
					  (setq f-value (funcall funcao res))
					  (setq no (make-node :estado res :valor-funcao f-value :accao-origem (nth i accoes-possiveis) :node-pai node))
					  (setq fronteira (insere no fronteira))))
	(return-from expande-e-insere fronteira)))
	
	

;;; insere: no x lista --> lista
;;; funcao que recebe um no e uma lista ordenada por valor de f crescente,
;;; e insere esse no na lista recebida, de modo igualmente ordenado.
;;; No fim, devolve essa nova lista.				  
(defun insere (no fronteira)
  (cond ((null fronteira) (list no))
        ((<= (node-valor-funcao no) (node-valor-funcao (car fronteira))) (cons no fronteira))
        (t (cons (car fronteira) (insere no (cdr fronteira))))))
        

;;; constroi-accoes: no --> lista
;;; funcao que recebe um no e devolve uma lista com todas as accoes
;;; que deram origem a esse no, desde o estado inicial do problema.      
(defun constroi-accoes (no)
	(let ( (accoes nil) (no-aux no) )
		(loop while (not (equal (node-node-pai no-aux) nil)) do
			(progn  (setq accoes (append (list (node-accao-origem no-aux)) accoes)) (setq no-aux (node-node-pai no-aux)) ))
	(return-from constroi-accoes accoes)))
	

;;; funcao-a: problema x funcao --> funcao
;;; funcao que recebe um problema e uma funcao, correspondente a uma heuristica,
;;; e devolve uma outra funcao, correspondente a soma da funcao heuristica com o custo-caminho do problema.
;;; Trata-se da funcao a ser usada pela procura A*.
(defun funcao-a (problema heuristica)
	(return-from funcao-a (lambda (state) (+ (funcall heuristica state) (funcall (problema-custo-caminho problema) state)))))
	

;;; dfs-sem-heuristica: estado --> inteiro
;;; funcao que recebe um estado e, visto que a procura em profundidade primeiro 
;;; nao utiliza heuristicas, devolve simplesmente 0. Esta funcao permite essencialmente
;;; poupar codigo, ao nao ter de criar uma funcao especificamente para a procura-pp.
(defun dfs-sem-heuristica (state)
	(declare (ignore state))
	(return-from dfs-sem-heuristica 0))
	

;;; heuristica-op: estado --> inteiro
;;; funcao que recebe um estado, e devolve o valor da funcao heuristica
;;; a ser usada no procura-best, aplicada a esse estado.
(defun heuristica-op (state)
	(let ((a 20) (b 1) (c 1) (d 1))
		(return-from heuristica-op (+ (* a (altura-total state)) (* b (linhas-completas state)) (* c (buracos state)) (* d (desnivel state))))))
		

;;; altura-total: estado --> inteiro
;;; funcao que recebe um estado e devolve a soma das alturas das 10 colunas
;;; do tabuleiro desse estado.	
(defun altura-total (state)
	(let ((tab (estado-Tabuleiro state)) (soma-altura 0))
		(loop for coluna from 0 to 9 do (setq soma-altura (+ soma-altura (1- (tabuleiro-altura-coluna tab coluna)))))
		(return-from altura-total soma-altura)))
		

;;; linhas-completas: estado --> inteiro
;;; funcao que recebe um estado e devolve o numero de linhas completas
;;; do tabuleiro desse estado.		
(defun linhas-completas (state)
	(let ((completas 0) (tab (estado-Tabuleiro state)))
		(loop for linha from 0 to 17 do
			(progn (if (tabuleiro-linha-completa-p tab linha) (setf completas (1+ completas)))))
		(return-from linhas-completas completas)))
		

;;; buracos: estado --> inteiro
;;; funcao que recebe um estado e devolve o numero de buracos
;;; do tabuleiro desse estado. E considerado um buraco uma posicao vazia
;;; tal que, acima dessa posicao vazia, existe pelo menos uma posicao preenchida na mesma coluna.		
(defun buracos (state)
	(let ((tab (estado-Tabuleiro state)) (nBuracos 0))
		(loop for col from 0 to 9
			do (progn (loop for l from 0 to 16
				do (if (and (not (aref tab l col)) (aref tab (1+ l) col)) (setq nBuracos (1+ nBuracos))))))
	(return-from buracos nBuracos)))
	
	
;;; desnivel: estado --> inteiro
;;; funcao que recebe um estado e devolve o desnivel do tabuleiro
;;; ou seja, a soma do valor absoluto das diferencas de alturas
;;; entre duas colunas consecutivas.	
(defun desnivel (state)
	(let ((tab (estado-Tabuleiro state)) (soma 0))
		(loop for col from 0 to 8 do (setq soma (+ soma (abs (- (tabuleiro-altura-coluna tab col) (tabuleiro-altura-coluna tab (1+ col)))))))
	(return-from desnivel soma))) 
		

;;; copy-array: array --> array
;;; funcao que recebe um array e retorna outra array, correspondente a copia do array original.	
(defun copy-array (array)
 (let ((dims (array-dimensions array)))
   (adjust-array
    (make-array dims :displaced-to array)
    dims)))
    

;;; remove-e-calcula: estado --> {}
;;; funcao que recebe um estado e remove todas as linhas totalmente preenchidas,
;;; calculando o numero de pontos de acordo com o que foi descrito no enunciado,
;;; e modificando o estado.
(defun remove-e-calcula (state)
	(let ((contador 0))
		(loop for linha from 0 to 17
			do (if (tabuleiro-linha-completa-p (estado-Tabuleiro state) linha) (progn (tabuleiro-remove-linha! (estado-Tabuleiro state) linha) (decf linha) (incf contador))))
		(cond ((equal contador 1) (setf (estado-pontos state) (+ (estado-pontos state) 100)))
			  ((equal contador 2) (setf (estado-pontos state) (+ (estado-pontos state) 300)))
			  ((equal contador 3) (setf (estado-pontos state) (+ (estado-pontos state) 500)))
			  ((equal contador 4) (setf (estado-pontos state) (+ (estado-pontos state) 800))))))
			  

;;; max-pontuacao-peca: array --> inteiro
;;; funcao que recebe um array correspondente a uma peca e devolve a pontuacao maxima
;;; para aquela peca, de acordo com o que foi descrito no enunciado.
(defun max-pontuacao-peca (peca)
	(cond ((equal peca 'i) (return-from max-pontuacao-peca 800)) 
		  ((equal peca 'j) (return-from max-pontuacao-peca 500))
		  ((equal peca 'l) (return-from max-pontuacao-peca 500))
		  ((equal peca 's) (return-from max-pontuacao-peca 300))
		  ((equal peca 'z) (return-from max-pontuacao-peca 300))
		  ((equal peca 't) (return-from max-pontuacao-peca 300))
		  ((equal peca 'o) (return-from max-pontuacao-peca 300))))
		  

;;; pode-colocar-peca: estado x accao x inteiro --> valor booleano
;;; funcao que recebe um estado, uma accao e um inteiro correspondente a uma linha
;;; e retorna o valor logico verdade caso seja possivel colocar a peca naquela linha
;;; tendo em conta o preenchimento do tabuleiro e os limites, tal como
;;; descrito no enunciado.
(defun pode-colocar-peca (state accao linha)
 (let ((peca (cdr accao)) (coluna (car accao)) (tabuleiro (tabuleiro->array (estado-Tabuleiro state))) (nColunas (array-dimension (cdr accao) 1)) (nLinhas (array-dimension (cdr accao) 0)))
  (if (> (+ coluna (1- nColunas)) 9) (return-from pode-colocar-peca nil))
  (loop for l from 0 to (1- nLinhas)
   do (progn (loop for col from 0 to (1- nColunas)
    do (progn (if (and (aref peca l col) (aref tabuleiro (+ linha l) (+ coluna col))) (return-from pode-colocar-peca nil))
        (if (> (1+ (+ linha l)) 17) (return-from pode-colocar-peca t))))))
  (return-from pode-colocar-peca t)))
  
  
;;; coloca-peca: estado x accao x inteiro --> {}
;;; funcao que recebe um estado, uma accao e um inteiro correspondente a uma linha
;;; e coloca a peca contida em accao na coluna e na linha especificadas,
;;; modificando assim o tabuleiro e o estado recebido.
(defun coloca-peca (state accao linha)
 (let ((peca (cdr accao)) (coluna (car accao)) (tabuleiro (tabuleiro->array (estado-Tabuleiro state))) (nColunas (array-dimension (cdr accao) 1)) (nLinhas (array-dimension (cdr accao) 0)))
  (if (> (+ linha (1- nLinhas)) 17) (setf (aref tabuleiro 17 0) t)
   (loop for l from 0 to (1- nLinhas)
    do (progn (loop for col from 0 to (1- nColunas)
     do (progn (setf (aref tabuleiro (+ linha l) (+ coluna col)) (or (aref tabuleiro (+ linha l) (+ coluna col)) (aref peca l col))))))))
  (setf (estado-Tabuleiro state) (array->tabuleiro tabuleiro))))
  

;; acrescentei algumas funcoes auxiliares que vao dar jeito para testar automaticamente o codigo dos alunos
(defun ignore-value (x)
	(declare (ignore x))
	'ignore)

;;; random-element: list --> universal
;;; funcao que dada uma lista, devolve um elemento aleatorio dessa lista
;;; se a lista recebida for vazia, e devolvido nil
(defun random-element (list)
  (nth (random (length list)) list))

;;; random-pecas: inteiro --> lista
;;; funcao que recebe um inteiro que especifica o numero de pecas pretendidas, e devolve uma lista com
;;; pecas (representadas atraves de um simbolo) escolhidas aleatoriamente. O tamanho da lista devolvida corresponde
;;; ao inteiro recebido.
(defun random-pecas (n)
	(let ((lista-pecas nil))
		(dotimes (i n)
			(push (random-element (list 'i 'l 'j 'o 's 'z 't)) lista-pecas))
		lista-pecas))
		
;;; cria-tabuleiro-aleatorio: real (opcional) x real (opcional) --> tabuleiro
;;; funcao que recebe um valor real (entre 0 e 1) para a probabilidade a ser usada na primeira linha e outro real
;;; que representa o decrescimento de probabilidade de uma linha para a seguinte. Estes argumentos sao opcionais,
;;; e se nao forem especificados tem o valor por omissao de 1.0 (100%) e 0.05 (5%) respectivamente
;;; A funcao retorna um tabuleiro em que cada posicao foi preenchida de acordo com as probabilidades especificadas
;;; para cada linha. A linha inicial tera uma maior probabilidade, mas as linhas seguintes terao uma menor probabilidade
;;; de preenchimento, resultado em media mais posicoes preenchidas no fundo do tabuleiro do que no topo. 
(defun cria-tabuleiro-aleatorio (&optional (prob-inicial 1.0) (decaimento 0.05))
	(let ((tabuleiro (cria-tabuleiro))
		  (prob prob-inicial)
		  (coluna-a-evitar 0))
		(dotimes (linha 18)
			;;;precisamos de escolher sempre uma coluna para nao preencher, se nao podemos correr o risco de criarmos uma linha
			;;;completamente preenchida
			(setf coluna-a-evitar (random 10)) 
			(dotimes (coluna 10)
				(when (and (not (= coluna-a-evitar coluna)) (<= (random 1.0) prob)) (tabuleiro-preenche! tabuleiro linha coluna)))
			;;;nao podemos permitir valores negativos de probabilidade
			(setf prob (max 0 (- prob decaimento))))
		tabuleiro))
		
;;; executa-jogadas: estado x lista --> inteiro
;;; funcao que recebe um estado e uma lista de accoes e executa as accoes (pela ordem recebida) sobre o tabuleiro do estado inicial,
;;; desenhando no ecra os varios estados do tabuleiro. Para avancar entre ecras, o utilizador deve premir a tecla "Enter".
;;;	retorna o total de pontos obtidos pela sequencia de accoes no tabuleiro
(defun executa-jogadas (estado-inicial lista-accoes)
	(let ((estado estado-inicial))
		(do () ((or (estado-final-p estado) (null lista-accoes)))
			(desenha-estado estado)
			(read-char)
			(desenha-estado estado (first lista-accoes))
			(read-char)
			(setf estado (resultado estado (first lista-accoes)))
			(setf lista-accoes (rest lista-accoes)))
		(desenha-estado estado)
		(estado-pontos estado)))

;;; desenha-estado: estado x accao (opcional) --> {}
;;; funcao que recebe um estado (e pode receber opcionalmente uma accao) e desenha o estado do jogo de tetris no ecra
;;; se for recebida uma accao, entao essa accao contem a proxima jogada a ser feita, e deve ser desenhada na posicao correcta por cima 
;;; do tabuleiro de tetris. Esta funcao nao devolve nada.		
(defun desenha-estado (estado &optional (accao nil))
	(let ((tabuleiro (estado-tabuleiro estado)))
		(desenha-linha-exterior) (format T "  Proxima peca:~A~%" (first (estado-pecas-por-colocar estado))) 
		(do ((linha 3 (- linha 1))) ((< linha 0))
			(desenha-linha-accao accao linha) (format T "~%"))
		(desenha-linha-exterior) (format T "  Pontuacao:~A~%" (estado-pontos estado))
		(do ((linha 16 (- linha 1))) ((< linha 0))
			(desenha-linha tabuleiro linha) (format T "~%"))
		(desenha-linha-exterior)))

;;; desenha-linha-accao: accao x inteiro --> {}
;;; dada uma accao e um inteiro correspondente a uma linha que esta por cima do tabuleiro (linhas 18,19,20,21) desenha
;;; a linha tendo em conta que podera estar la a peca correspondente a proxima accao. Nao devolve nada.
(defun desenha-linha-accao (accao linha)
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A " (cond ((null accao) " ")
							  ((and (array-in-bounds-p (accao-peca accao) linha (- coluna (accao-coluna accao)))
									(aref (accao-peca accao) linha (- coluna (accao-coluna accao)))) "#")
							  (T " "))))
	(format T "|"))
	
;;; desenha-linha-exterior: {} --> {}
;;; funcao sem argumentos que desenha uma linha exterior do tabuleiro, i.e. a linha mais acima ou a linha mais abaixo
;;; estas linhas sao desenhadas de maneira diferente, pois utilizam um marcador diferente para melhor perceber 
;;; os limites verticais do tabuleiro de jogo
(defun desenha-linha-exterior ()
	(format T "+-")
	(dotimes (coluna 10)
		(format T "--"))
	(format T "+"))
	
;;; desenha-linha-vazia: {} --> {}
;;; funcao sem argumentos que desenha uma linha vazia. Nao devolve nada.
(defun desenha-linha-vazia ()
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A "))
	(format T "|"))
	
;;; desenha-linha: tabuleiro,inteiro --> {}
;;; esta funcao recebe um tabuleiro, e um inteiro especificando a linha a desenhar
;;; e desenha a linha no ecra, colocando o simbolo "#" por cada posicao preenchida, 
;;; e um espaco em branco por cada posicao nao preenchida. Nao devolve nada.
(defun desenha-linha (tabuleiro linha)
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A " (if (tabuleiro-preenchido-p tabuleiro linha coluna) "#" " ")))
	(format T "|"))			

			
;;exemplo muito simples de um tabuleiro com a primeira e segunda linha quase todas preenchidas
(defvar t1 (cria-tabuleiro))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 0 coluna))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 1 coluna))
(defvar e1 (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t i)))

(defvar p1
	(make-problema :estado-inicial (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t i))
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))


#|(load "utils.fas")|#
