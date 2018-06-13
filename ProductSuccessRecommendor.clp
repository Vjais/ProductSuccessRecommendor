(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.*)
(load-package FuzzyFunctions)

(defglobal ?*qualt* =0.0)
(defglobal ?*compt* =0.0)
(defglobal ?*mrkt* =0.0)
(defglobal ?*Invest* =0.0)
(defglobal ?*rnd* =0.0)

(defglobal ?*result* = 0.0)

(defglobal ?*incp* = 0.0)
(defglobal ?*decm* = 0.0)
(defglobal ?*incm* = 0.0 )
(defglobal ?*decc* = 0.0)
(defglobal ?*iinc* = 0.0)

(defglobal ?*quality* = (new FuzzyVariable "quality" 0.5 2.5 "rating"))
(defglobal ?*competition* = (new FuzzyVariable "competition" 2.2 4.0 "intensity"))
(defglobal ?*marketing* = (new FuzzyVariable "marketing" 1.6 4.0 "strategy"))
(defglobal ?*Investment* = (new FuzzyVariable "Investment" 2.0 4.0 "amount"))
(defglobal ?*ResearchnDevelopment* = (new FuzzyVariable "ResearchnDevelopment" 0.8 3.0 "status"))


(defrule initialize-fuzzy-variables
    (declare (salience 100))
    =>
    (?*quality* addTerm "low" (new RightLinearFuzzySet 0.8 1.5))
   (?*quality* addTerm "medium" (new TriangleFuzzySet 1.5 1.6 2.0)) 
   (?*quality* addTerm "high" (new SFuzzySet 2.0 2.4))
	 
	
    
    (?*competition* addTerm "high" (new ZFuzzySet 2.4 2.8))
	(?*competition* addTerm "medium" (new TriangleFuzzySet 2.8 3.4 3.6))
	(?*competition* addTerm "low" (new SFuzzySet 3.6 4.0))
   
    
    (?*marketing* addTerm "low" (new ZFuzzySet 1.8 2.5))
	(?*marketing* addTerm "medium" (new TriangleFuzzySet 2.5 3.2 3.4))
	(?*marketing* addTerm "high" (new SFuzzySet 3.4 4.0))
    
    
     (?*Investment* addTerm "low" (new ZFuzzySet 2.0 2.7))
	(?*Investment* addTerm "medium" (new TriangleFuzzySet 2.7 3.1 3.4))
	(?*Investment* addTerm "high" (new SFuzzySet 3.4 4.0))
    
    
    (?*ResearchnDevelopment* addTerm "low" (new ZFuzzySet 1.0 1.6))
	(?*ResearchnDevelopment* addTerm "medium" (new TriangleFuzzySet 1.6 2.1 2.4))
	(?*ResearchnDevelopment* addTerm "high" (new SFuzzySet 2.4 3.0))
    
)

;
(defrule factor_values
    (declare (salience 50))
=>
    (assert (thequality (new FuzzyValue ?*quality* " medium")))
    (assert (thecompetition (new FuzzyValue ?*competition* "low")))
    (assert (themarketing (new FuzzyValue ?*marketing* "high")))
    (assert (theInvestment (new FuzzyValue ?*Investment* "high")))
    (assert (theResearchnDevelopment (new FuzzyValue ?*ResearchnDevelopment* "medium")))
    )

(defrule low_quality
    (declare (salience 40))
(thequality ?q&:(fuzzy-match ?q "low"))
     =>
    (bind ?*qualt*(?q momentDefuzzify) ) 
   
)

(defrule  medium_quality
     (declare (salience 40))
(thequality ?q&:(fuzzy-match ?q "medium"))
     =>
      (bind ?*qualt*(?q momentDefuzzify) ) 
    
    )

(defrule high_quality
     (declare (salience 40))
(thequality ?q&:(fuzzy-match ?q "high"))
     =>
  
    (bind ?*qualt*(?q momentDefuzzify) ) 
   
    )

(defrule low_competetion
     (declare (salience 40))
(thecompetition ?c&:(fuzzy-match ?c "low"))
     =>
     (bind ?*compt*(?c momentDefuzzify) )  
    )    

(defrule medium_competetion
     (declare (salience 40))
(thecompetition ?c&:(fuzzy-match ?c "medium"))
     =>
     (bind ?*compt*(?c momentDefuzzify) )  
    
    )    

(defrule high_competetion
     (declare (salience 40))
(thecompetition ?c&:(fuzzy-match ?c "high"))
     =>
     (bind ?*compt*(?c momentDefuzzify) )  
    
    )    

(defrule low_marketing
     (declare (salience 40))
(themarketing ?m&:(fuzzy-match ?m "low"))
     =>
     (bind ?*mrkt*(?m momentDefuzzify) )  
    
    )    

(defrule medium_marketing
     (declare (salience 40))
(themarketing ?m&:(fuzzy-match ?m "medium"))
     =>
     (bind ?*mrkt*(?m momentDefuzzify) )  
    
    )    

(defrule high_marketing
     (declare (salience 40))
(themarketing ?m&:(fuzzy-match ?m "high"))
     =>
     (bind ?*mrkt*(?m momentDefuzzify) )  
    
    )    

(defrule low_Investment
     (declare (salience 40))
(theInvestment ?i&:(fuzzy-match ?i "low"))
     =>
     (bind ?*Invest*(?i momentDefuzzify) )  
        )    

(defrule medium_Investment
     (declare (salience 40))
(theInvestment ?i&:(fuzzy-match ?i "medium"))
     =>
     (bind ?*Invest*(?i momentDefuzzify) )  
    )    

(defrule high_Investment
     (declare (salience 40))
(theInvestment ?i&:(fuzzy-match ?i "high"))
     =>
     (bind ?*Invest*(?i momentDefuzzify) )  
    )    

(defrule low_rnd
     (declare (salience 40))
(theResearchnDevelopment ?rd&:(fuzzy-match ?rd "low"))
     =>
        (bind ?*rnd*(?rd momentDefuzzify) )  
 
    )    

(defrule medium_rnd
     (declare (salience 40))
(theResearchnDevelopment ?rd&:(fuzzy-match ?rd "medium"))
     =>
      (bind ?*rnd*(?rd momentDefuzzify) )  
  
    )    

(defrule high_rnd
     (declare (salience 40))
(theResearchnDevelopment ?rd&:(fuzzy-match ?rd "high"))
     =>
    (bind ?*rnd*(?rd momentDefuzzify) )  
    
    )  

;adjusting decm factor required to calculate accurate result for various types of situation
 

(deffunction decm()
    (if(> ?*mrkt*  3.4 ) then 
     (bind  ?*decm* 0.6))
    else
    (if(> ?*mrkt* 2.0)then
    (bind ?*decm* 0.4))
 
   )
        
(defrule changem
    (declare (salience 2))
  (theResearchnDevelopment ?rd&:(fuzzy-match ?rd "low"))
        =>
     (decm())
      (printout t "changem has fired" crlf))

(deffunction decm1()
   
     (if(> ?*mrkt* 2.0)then
    (bind ?*decm* 0.25))
    else
     (if(> ?*mrkt*  3.4 ) then 
     (bind  ?*decm* 0.35))
   )

(defrule changem1
    (declare (salience 2))
    (theResearchnDevelopment ?rd&:(fuzzy-match ?rd "medium"))
        =>
     (decm1())
)
;adjusting incm factor required to calculate accurate result for various types of situation

(deffunction incm() 
    (if(> ?*Invest* 3.4)  then 
      (bind ?*incm*  1) 
    else
    (if(> ?*Invest* 2.7))then
    (bind ?*incm* 0.9))
    
   )

(defrule changem2
    (declare (salience 2))
    (themarketing ?m&:(fuzzy-match ?m "low"))
    
        =>
     (incm())
)

(deffunction incm2() 
    (if(> ?*Invest* 3.4)  then  
      (bind ?*incm*  0.4) 
    else
    (if(> ?*Invest* 2.7))then
    (bind ?*incm* 0.4))
    
   )

(defrule changem3
    (declare (salience 2))
(themarketing ?m&:(fuzzy-match ?m "medium"))
        =>
     (incm2())
     )

(deffunction incm3() 
    (if(> ?*Invest* 3.4)  then  
      (bind ?*incm*  0.3) 
    else
   (if(> ?*Invest* 2.7))then
    (bind ?*incm* 0.2))
    
   )

(defrule changem4
    (declare (salience 2))
   (themarketing ?m&:(fuzzy-match ?m "high"))
        =>
     (incm3())
      )

;adjusting incp factor required to calculate accurate result for various types of situation

(deffunction incp()
    (if(> ?*Invest* 3.4)  then  
     (bind  ?*incp* 0.5))
    else
    (bind ?*incp* 0.3)
    )

(defrule changep
    (declare (salience 2))
    (themarketing ?m&:(fuzzy-match ?m "high"))
        =>
     (incp())
     )

(defrule changep2
    (declare (salience 2))
    (thecompetition ?c&:(fuzzy-match ?c "low"))
        =>
     (bind  ?*incp* 1)
      )

(defrule changep4
    (declare (salience 2))
    (thecompetition ?c&:(fuzzy-match ?c "high"))
        =>
    (if(> ?*Invest* 3.4)then
     (bind  ?*incp* 0.6))
    (if (> ?*mrkt* 2.7)then
     (bind ?*decm* 0.1))
    (if (> ?*mrkt* 3.4)then
     (bind ?*decm* 0.3))
     
      )

(defrule changep3
    (declare (salience 2))
   (thecompetition ?c&:(fuzzy-match ?c "medium"))
          
     =>
   (if(> ?*Invest* 3.4)then
     (bind  ?*incp* 0.9))
(if (> ?*mrkt* 2.7)then
     (bind ?*decm* 0.2))
(if (> ?*mrkt* 3.4)then
     (bind ?*decm* 0.2))
    )


(deffunction findresult()
  
  
  (bind ?result2(*(+(+ ?*compt* ?*Invest*)?*incp*)0.5))
    (bind ?result1 (+(+ ?*qualt* ?*rnd*)?*iinc*))
    (bind ?result3 (-(+ ?*mrkt* ?*incm* )?*decm*))
        (bind ?*result* (*(* ?result1 ?result2)?result3))
    (printout t "" crlf)
   (printout t "Predicted success rate for your product is: " ?*result* " %" crlf )
    )

(deffunction giveadvice()
  (if(< ?*result* 45) then
        (printout t "The success rate for your product is low" crlf)
        (printout t "We recommend you to look into our suggestions " crlf))
    else(if (< ?*result* 65)then(if(> ?*result* 55)then
        (printout t "The success rate for your product is considerable" crlf)
        (printout t "We still recommend you to look into our suggestions " crlf)))
  else (if(> ?*result* 75) then
   (printout t "Hence your decision is optimistic you can go ahead" crlf)))
    
(deffunction givesuggestion()
    (printout t"" crlf)
        (if(< ?*mrkt* 3.4 ) then
        (printout t "Suggestion: " )
      (printout t "Try to improve your marketing strategies" crlf))
    (if(< ?*Invest* 3.4 ) then
        (printout t "Suggestion: " )
      (printout t "Try to improve Investment by more advertising" crlf))
       ( if(< ?*qualt* 2.0) then
    (printout t "Suggestion: " )
    (printout t "Try to improve quality to attract more customers" crlf))
    (if (< ?*compt* 2.8)then 
     (printout t "Suggestion: ")
    (printout t "Due to high intensity of competiton find strategies to excel")))
    

(reset)
(run)
(findresult())
(giveadvice())
(givesuggestion())