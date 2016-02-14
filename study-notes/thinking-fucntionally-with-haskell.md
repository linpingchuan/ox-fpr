# What is this?
Study notes from Richard Bird's Functional Programming with Haskell text book.

# What is Functional Programming?
* Haskell notation of a function
        
        f :: X -> Y
        
* `Integer` - type for unlimited precision integers; `Int` - type for limited precision integers

* Functional application in haskell associates to the left    
`log sin x` is read as `(log sin) x`. To convey intended meaning, parenthesis are used, `log (sin x)`

* Functional application has the highest binding power. Thus, functional application binds tighter than multiplication, 
addition etc.   
Thus, `sin theta * cos theta` does not need parenthesis and is equivalent to `(sin theta) * (cos theta)`
  
* Functional composition

        Given f :: Y -> Z and g :: X -> Y
        then f . g :: X -> Z
        
* Order of composition is from __right to left__
        
        (f . g) x = f (g x)      
    * So `A -> B -> C` means `A -> (B -> C)`
    
* Haskell has support for type synonyms, example:
    
        type word = [Char]
        
* Example: Common words (Refer sec 1.3 of [1])
        
        # Function declaration and definition
        commonWords :: Int -> Text -> Words
        commonWords n = concat 
                        . map showRun 
                        . take n 
                        . sortRuns 
                        . countRuns 
                        . sortWords 
                        . words 
                        . map toLower
        
        # Function application                                
        commonWords 2 "Bla bla BLA ra da Ra"
        
        =>
        
        concat 
            . map showRun 
            . take 2 
            . sortRuns 
            . countRuns 
            . sortWords 
            . words 
            . map toLower
        "Bla bla BLA ra da Ra"

        =>
        
        concat 
            . map showRun 
            . take 2 
            . sortRuns 
            . countRuns 
            . sortWords 
            . words
        "bla bla bla ra da ra"
        
        =>
        
        concat 
            . map showRun 
            . take 2 
            . sortRuns 
            . countRuns 
            . sortWords 
        ["bla", "bla", "bla", "ra", "da", "ra"]
        
        =>
        concat 
            . map showRun 
            . take 2 
            . sortRuns 
            . countRuns 
        ["bla", "bla", "bla", "da", "ra", "ra"]
                
        =>                
        concat 
            . map showRun 
            . take 2 
            . sortRuns 
        [("bla", 3), ("da", 1), ("ra", 2)]
                                                            
        =>

        concat 
            . map showRun 
            . take 2 
        [("bla", 3), ("ra", 2), ("da", 1)]
                                                                                                
        =>
        
        concat 
            . map showRun
        [("bla", 3), ("ra", 2)]
                            
        =>
        
        concat
        ["bla 3\n", "ra 2\n"]

        =>
    
        "bla 3\n ra 2\n"                                                                                                                                                    
                                                
         
# References
* [1] : Thinking functionally with haskell, Richard Bird
        
                
        
        