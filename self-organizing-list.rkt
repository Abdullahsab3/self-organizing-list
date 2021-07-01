#lang r6rs

(library
 (self-organizing-list)
 (export new from-scheme-list self-organizing-list? empty? full? length
         find! delete! add! peek
         set-current-to-first! set-current-to-next! has-current? current-has-next?)

 (import (except (rnrs base) length)
         (srfi :9)
         (srfi :19)
         (rnrs io simple))

 
 ;; 1.1 Onderdeel 1: implementeer self-organizing lists

 
 (define-record-type self-organizing-list
   (make-self-organizing-list s h c e l)
   self-organizing-list?
   (s size size!)
   (h head head!)
   (c current current!)
   (e equality)
   (l lesser))

 
 (define-record-type list-node
   (make-list-node v p n t)
   list-node?
   (v list-node-val list-node-val!)
   (p list-node-prev list-node-prev!)
   (n list-node-next list-node-next!)
   (t list-node-time list-node-time!))
 
 (define (new ==? <<?)
   (make-self-organizing-list 0 '() '() ==? <<?))
 
 (define (from-scheme-list slst ==? <<?)
   (let list-rec
     ((lst slst))
     (if (null? lst)
         (new ==? <<?)
         (add! (list-rec (cdr lst)) (car lst)))))



 (define (empty? solst)
   (= (size solst) 0))
 
 (define (length solst)
   (size solst))

 ;; full geeft altijd #f terug omdat .. 
 ;; .. de implementatie altijd nodes kan aanmaken en aan de lijst toevoegen..
 ;; .. zolang er voldoende geheugen is.
 (define (full? solst)
   #f)

 (define (find! solst value)
   (define ==? (equality solst))
   (let list-iter
     ((node (head solst)))
     (cond
       ((null? node) 
        (current! solst '()))
       ((==? value (list-node-val node))
        ;; indien de value gevonden wordt in de lijst, ..
        ;; .. pas de tijd van de node aan naar de tijd van het huidige moment.
        (list-node-time! node (time-second (current-time)))
        (current! solst node)
        ;; verwijder de node en voeg het opnieuw toe in de lijst ..
        ;; met de tijd en de value.
        ;; Dit zorgt voor minder code duplicatie in het adt en de genericiteit blijft bewaard.
        (let* ((temp (current solst))
               (val (list-node-val temp))
               (time (list-node-time temp)))
          (delete! solst)
          (add! solst val time)))
       (else
        (list-iter (list-node-next node)))))
   solst)


 (define (delete! solst)
   (define curr (current solst))
   (if (not (has-current? solst))
       (error "no current (delete!)" solst))
   (cond ((not (current-has-next? solst))
          ;; als de current geen next heeft, ..
          ;; .. wijst de current naar het laatste element van de lijst.
          (list-node-next! (list-node-prev curr) '()))
         ((not (current-has-previous? solst))
          ;; als de current geen previous heeft,..
          ;; .. wijst de current naar het eerste element van de lijst.
          (head! solst (list-node-next curr))
          (list-node-prev! (list-node-next curr) '()))
         ;; het element verwijderen door de next pointer van het element voor de current ..
         ;; .. te laten wijzen naar het element na de current,
         ;; en de previous pointer van het element na de current te laten wijzen naar ..
         ;; .. het element voor de current
         (else (list-node-next! (list-node-prev curr) (list-node-next curr))
               (list-node-prev! (list-node-next curr) (list-node-prev curr))))
   (size! solst (- (size solst) 1))
   (current! solst '())
   solst)
 
 (define (peek solst)
   (if (not (has-current? solst))
       (error "no current (peek)" solst)
       (list-node-val (current solst))))

 (define (add! solst val . argls)
   ;; add! neemt een verplicht argument, de value en een optioneel argument, de tijd.
   ;; Wanneer add! binnen de find! opgeroepen wordt, wordt een nieuwe node aangemaakt ..
   ;; .. met value en tijd, en deze node wordt aan de lijst toegevoegd.
   ;; Anders wordt time aan 0 gebonden.
   (define time (if (not (null? argls))
                    (car argls)
                    0))
   (define <<? (lesser solst))
   (define (insert-node prev next! next)
     (let ((node (make-list-node val prev next 0)))
       (current! solst node)
       (next! prev node)
       (if (current-has-next? solst)
           (list-node-prev! next node))))
   (define (iter-to-position prev next! next)
     (cond
       ((or (null? next)
            ;; als add! opgeroepen wordt zonder Time mee te geven ..
            ;; .. (om dus een element toe te voegen aan de lijst met een value) ..
            ;; .. dan wordt het derde argument van de <<? ook 0.
            ;; Het toegevoegde element wordt dan aan de lijst toegevoegd en alleen de values worden vergeleken.
            (<<? val time (list-node-val next) (if (zero? time)
                                                   0
                                                   (list-node-time next))))
        (insert-node prev next! next))
       (else
        (iter-to-position next list-node-next! (list-node-next next)))))
   (iter-to-position
    '()
    (lambda (ignore node) (head! solst node))
    (head solst))
   (size! solst (+ (length solst) 1))
   solst)

 ;; Aangezien dit een dubbelgelinkte implementatie is,..
 ;;.. wordt het ADT van extra naviagatie en verificatie procedures voorzien.
 ;; Zoals: set-current-to-previous! om de current te laten wijzen naar de previous node ..
 ;; .. van de "oude" current.
 
 (define (set-current-to-first! solst)
   (current! solst (head solst)))
 
 (define (set-current-to-next! solst)
   (if (not (and (current-has-next? solst) (has-current? solst)))
       (error "current has no meaningful value (set-current-to-next!" solst)
       (current! solst (list-node-next (current solst)))))


 (define (set-current-to-previous! solst)
   (if (not (and (current-has-previous? solst) (has-current? solst)))
       (error "current has no meaningful value (set-current-to-previous!" solst)
       (current! solst (list-node-prev (current solst)))))

 
 (define (has-current? solst)
   (not (null? (current solst))))
 
 (define (current-has-next? solst)
   (if (not (has-current? solst))
       (error "no Current (current-has-next?" solst)
       (not (null? (list-node-next (current solst))))))

 (define (current-has-previous? solst)
   (if (not (has-current? solst))
       (error "no current (current-has-next?" solst)
       (not (null? (list-node-prev (current solst))))))
 
 ;; Eén procedure die gebruikt kan worden om een solst te maken ..
 ;; waarbij de elementen die het meest recent gezocht worden vooraan in de lijst ..
 ;; .. komen te staan.
 (define (vergelijkingstest-meest-recent-vooraan ignore t1 ignore2 t2)
    (>= t1 t2))
 

 ;;; testprocedures
 
 ;; loopen over de hele lijst en de values van alle elementen displayen ..
 ;; .. om te controleren of alle next pointers in orde zijn.
 (define (loop-over-de-lijst solst)
   (set-current-to-first! solst)
   (let loop
     ((lst solst)
      (length (length solst)))
     (if (and (not (zero? length)) (current-has-next? solst))
         (begin (display (peek solst))
                (display "  ")
                (set-current-to-next! solst)
                (loop lst (- length 1)))
         ;; als de current geen next heeft, is de current het laatste element van de lijst.
         ;; Dus moeten we de current nog peeken.
         (display (peek solst)))))
 
 ;; loopen over de hele lijst en dan terug loopen vanachter om zowel de next-pointers ..
 ;; als de prev-pointers te controleren.
 (define (loop-over-de-lijst-heen-en-weer solst)
   (display "de lijst aflopen:            ")
   (loop-over-de-lijst solst)
   (newline)
   (display "de lijst vanachter aflopen:  ")
   (let loop
     ((lst solst)
      (length (length solst)))
     (if (current-has-previous? lst)
         (begin (display (peek lst))
                (display "  ")
                (set-current-to-previous! lst)
                (loop lst (- length 1)))
         ;; Als de current geen previous heeft, is de current het eerste element van de lijst.
         ;; Dus moeten we de current nog peeken.
         (display (peek lst))))))