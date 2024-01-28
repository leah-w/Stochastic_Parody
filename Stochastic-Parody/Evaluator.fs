module Evaluator
open AST
open System.IO
open Sentiments
open System.Collections

// lets put everything in twice --> once with real rhyme match the other with 
// "" rhyme match to match w things that dont need to rhyme
type feature = {emph: string; pos: string; rhyme: string}
// hashmaps are word -> feature


// flag to show how the translation is done
// let VERBOSE = true

// hashtable storing translations we have already used
let reuse = new Hashtable()

// helper method to add an element to a hashtable with a list as its value
let update_hashmap (map:Hashtable) key toadd =         
    if map.ContainsKey(key) then
        let old: 'a list = map[key] |> unbox
        map.Remove(key)
        map.Add(key, box (toadd::old))

    else
        map.Add(key, box [toadd])

// helper to change the values of a given key
let update_remove_hashmap (map:Hashtable) key toadd = 
    if map.ContainsKey(key) then
        // if we remove the next two lines vars are immutable
        map.Remove(key)
        map.Add(key, box toadd)

    else
        map.Add(key, box toadd)

(*
* Function that parses all of our dictionary files and returns a series of hashtables that contain all the data. 
* Warns if words in the preferred array (keywords) aren't in the CMU dict.
* @param preferredArr: An array of words that is the combination of sentiment and keywords we will build a dictionary out of them
* @return a 4-tuple of all of the dictionaries we need: one that maps words to their features for the first step of translation
* and then 3 that map features to the list of words that match that feature for the preferred, common and whole dictionary
*)
let readDict (preferredArr: string list) =
    let wordToFeature = new Hashtable()
    let featToWord = new Hashtable()
    let commonFeatToWord = new Hashtable()
    let preferredFeatToWord = new Hashtable()

    let cmu_file = Path.Combine(__SOURCE_DIRECTORY__, "new_cmu_pos_dict.txt")
    let common_file = Path.Combine(__SOURCE_DIRECTORY__, "new_common.txt")


    // reading in files
    if (not (File.Exists(cmu_file) && File.Exists(common_file))) then
        printfn "Error could not find Dictionary Files"
        printfn "Exiting"

        exit(-2)
    
    let dict = File.ReadAllText cmu_file
    let common = File.ReadAllText common_file

    let dictArr = dict.Split('\n')
    let commonArr = common.Split('\n')

    // fill the CMU dict hashtable
    let dummy = dictArr |> Array.map (fun i -> 
        let spl: string list = i.Split(' ') |> Array.toList
        let len = (List.length spl)
        // we consider the rhyme the last 2 syllables matching unless there are 2 or less syllables then it is only the last
        let myRhyme:string list = (if len <= 3 then [spl[len - 1]] else [spl[len - 2] ; spl[len - 1]])
        let myStringRhyme = String.concat "" myRhyme 

        // POS = part of speech and is added to the dictionary in preprocessing in this slot
        let myPOS = spl[1]

        // for the emphasis we only care about the number (which signifies stress) thus we us a map to extract and then recombine
        let myEmphPreCat = spl[2..] |> List.map (fun s ->
            if s.Contains("2") then
                "2"
            else if s.Contains("1") then
                "1" 
            else if s.Contains("0") then
                "0" 
            else "")
        let myEmph = myEmphPreCat |> String.concat ""

        // setting up the 2 feature records
        let myFeature = {emph = myEmph; pos = myPOS; rhyme = myStringRhyme}
        let myEmphPOSandNoRhyme = {emph = myEmph; pos = myPOS; rhyme = ""}

        // update the hashtable
        wordToFeature.Add(spl[0], box myFeature)
        update_hashmap featToWord myFeature spl[0]
        update_hashmap featToWord myEmphPOSandNoRhyme spl[0]

        )

    // fill the common array hashtable by querying the now existing CMU hastable
    let dummy = commonArr |> Array.map (fun i ->

        // warn if it is not in the CMU dict --> if not it the dict we dont know its features
        if wordToFeature.ContainsKey((i.ToUpper())) then
            let feat: feature = wordToFeature[(i.ToUpper())] |> unbox
            let noRhymeFeat: feature = {emph = feat.emph; pos = feat.pos; rhyme = ""}
            update_hashmap commonFeatToWord feat (i.ToUpper())
            update_hashmap commonFeatToWord noRhymeFeat (i.ToUpper())
        else 
            printfn "Warning: %A does not exist in the dictionary" i

        )
    // fill the preferred array hashtable by querying the now existing CMU hastable
    let dummy = preferredArr |> List.map (fun i ->
        if wordToFeature.ContainsKey((i.ToUpper())) then
            let feat: feature = wordToFeature[(i.ToUpper())] |> unbox
            let noRhymeFeat:feature = {emph = feat.emph; pos = feat.pos; rhyme = ""}

            update_hashmap preferredFeatToWord feat (i.ToUpper())
            update_hashmap preferredFeatToWord noRhymeFeat (i.ToUpper())
        else 
            printfn "Warning: %A does not exist in the dictionary" i
        )
    
    // return
    (wordToFeature, featToWord, commonFeatToWord, preferredFeatToWord)

(*
* Evaluator for sentiments, takes in a string that is a valid sentiment and returns the list of words that go with that 
* Sentiment, warns if the sentiment is not valid.
* @param sent: A string that reprisents the sentiment you want to have
* @return a list of words that match the requested sentiment
*)
let evalSentiment sent = 
    if sent = "happy" then happy_sentiment
    else if sent = "depressing" then depressing_sentiment
    else if sent = "angry" then angry_sentiment
    else if sent = "funny" then funny_sentiment 
    else 
        if sent <> "" then printfn "Warning %A is not a valid sentiment at this time" sent
        []

(*
* Function that translates a given line into according to user specifications
* Warns if words cannot be translated
* @param lines: A list of TranslationUnit (words with with instructions on how to translate) to be translated
* @param wtf: A hashtable that maps words to features
* @param ftw: A hashtable that maps features to all words in the whole CMU dict that match that feature
* @param cftw: A hashtable that maps features to common words that match that feature
* @param pftw: A hashtable that maps features to the preferred words that match that feature
* @return a list of strings of the translated words
*)
let convert (lines: TranslationUnit list) (wtf: Hashtable) (ftw: Hashtable) (cftw: Hashtable) (pftw: Hashtable) (verboseFlag: bool): string list=

    let rnd = System.Random()

    // go throught the whole list
    let rec helpConvert (ls: TranslationUnit list) = 
        match (ls: TranslationUnit list) with 
        | [] -> []
        | x::xs ->
                // if we are supposed to translate see if we have translated this word before
                // if so then just return the old translation
                if x.translate = true then
                    if reuse.ContainsKey(x.word.ToLower()) then 
                        let reusedWord = reuse[x.word.ToLower()] |> unbox
                        reusedWord::(helpConvert xs)
                    else
                        // if the word is not contained in our dictionary give a warning
                        if wtf.ContainsKey(x.word.ToUpper()) = false then 
                                printfn "Word %A is not contained in the CMU Dictionary" x.word
                                printfn "Word can still exist in input but cannot be translated"
                                printfn "Remove translation flag and try again"
                                printfn "Exiting"
                                exit(-1)
                        
                        // get the feature of the word and remove the rhyme if we don't want to match rhyme
                        let myFeat: feature = wtf[(x.word.ToUpper())] |> unbox
                        let correctFeat = if (x.rhyme) then myFeat else {emph = myFeat.emph; pos = myFeat.pos; rhyme = ""}
                        // determine which translator to use, order is preferred then common then general
                        let translator, tid = (
                            if (pftw.ContainsKey(correctFeat) && 
                                ((((unbox pftw[correctFeat]): string list).Length > 1) || ((unbox pftw[correctFeat]): string list)[0] <> x.word.ToUpper())) then
                                pftw, "Priority"
                            else if (cftw.ContainsKey(correctFeat) && 
                                ((((unbox cftw[correctFeat]): string list).Length > 1) || ((unbox cftw[correctFeat]): string list)[0] <> x.word.ToUpper())) then

                                cftw, "Common"
                            else if (ftw.ContainsKey(correctFeat)) then
                                ftw, "General"
                            else
                                printfn "Word %A could not be translated because feat %A DNE" x.word correctFeat
                                printfn "Word can still exist in input but cannot be translated"
                                printfn "Remove translation flag and try again"
                                printfn "Exiting"
                                exit(-1)
                            )

                        // get a random word that matches the feature
                        let wordList: string list = (translator[correctFeat]) |> unbox
                        let len = wordList.Length
                        let ind = rnd.Next(len)
                        let newWord = wordList[ind] 
                        // adjust if we get the same word back that we put in
                        let fixedWord = 
                            if (newWord.ToLower() = x.word.ToLower()) then
                                if (len > 1) then
                                    wordList[(ind + 1) % len]
                                else 
                                    printfn "Warning: no match was found for %A" x.word
                                    newWord
                            else 
                                newWord

                        // remove the word we translated the inital word to from the potential words and add it the the current mappings
                        let updatedWordList = wordList |> List.filter(fun x -> x <> fixedWord)
                        translator.Remove(correctFeat)
                        if updatedWordList.Length <> 0 then
                            translator.Add(correctFeat, updatedWordList)
                        reuse.Add(x.word.ToLower(), fixedWord)
                        
                        // this is just a verbose flag
                        if verboseFlag then printfn "%A, %A, %A, %A" x.word fixedWord correctFeat tid
                        
                        // add the word to the list
                        fixedWord::(helpConvert xs)
                else 
                    // word is not supposed to be translated append as is
                    x.word::(helpConvert xs)
                    
    helpConvert lines

(*
* Function that evaluates a given program AST that is generated after being parsed
* note keywords and sentiment have to be the first two things parsed
* @param ps: a list of expressions that reprisents each part of the AST
* @return a list of list strings that reprisent all the lines of the newly translated song
*)
let evalProg (ps: Grammar list) (verbose: bool) = 

    // generate the preferred word list anything that is not keywords or sentiment is irrelevant
    let wordList = 
        (match ps[0] with
        | Sentiment x -> evalSentiment x
        | Keywords x -> x
        | _ -> []
        )
        @
        (match ps[1] with
        | Sentiment x -> evalSentiment x
        | Keywords x -> x
        | _ -> []
        )

    // this hashtable stores all of our sections (variables)
    let mySections = new Hashtable() // string (section_name) -> Line List


    if verbose then printfn "Word List: %A" wordList
    if verbose then printfn "Assembing Dictionary"
    let wtf, ftw, cftw, pftw = readDict wordList
    if verbose then printfn "Dictionary Complete"
    let rec matchAbstractType (ls: Grammar list) = 
        match ls with
        | [] -> []
        | x::xs ->
            match x with
            // if it is a section declaration then add the section to the hashtable
            | Section (name, lines) -> 
                update_remove_hashmap mySections name lines
                matchAbstractType xs

            // if it is a instance of a section the parse each line and add them all
            | Section_Instance var ->
                if mySections.ContainsKey(var) then 
                    let lines: Grammar List = mySections[var] |> unbox
                    // parse each line
                    let linesToAdd: string list list = 
                        List.foldBack
                            (fun ex acc ->
                            (match ex with
                            // there should only be lines in a section
                            | Line l -> (convert l wtf ftw cftw pftw verbose)
                            | _ -> 
                                printfn "Warning should only be lines in a section"
                                []
                            ) :: acc) lines [] 
                    
                    // add to list
                    linesToAdd @ matchAbstractType xs
                else
                    printfn "Section %A is undeclared and cannot be referenced" var
                    printfn "Try declaring the section before you reference it"
                    printfn "Exiting"

                    exit(-1)

            // converts a single line
            | Line x ->
                (convert x wtf ftw cftw pftw verbose) :: matchAbstractType xs

            // does nothing if sees keywords or sentiment
            | _ -> matchAbstractType xs

    matchAbstractType ps

(*
* Function that prints the result of the evaluation nicely as lines of a song
* @param p: a list lines (lists of strings) to print nicely
* @return a set of lyrics
*)
let rec prettyprint (p: string list list): string =
    match p with
    | [] -> ""
    | x::xs ->( x |> String.concat " ").ToLower() + "\n" + (prettyprint xs)