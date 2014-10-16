package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val occurrenceList = w.toLowerCase.toList.groupBy((element: Char) => element).toList
	(for(elem <- occurrenceList) yield (elem._1, elem._2.length)).sortBy(_._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s mkString "")

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(word => wordOccurrences(word)) 
  
  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List[Word]())

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =  {
    def combine(in: String): List[Word] = {
      for {
        len <- List.range(0, in.toList.length+1)
        comb <- in.toList.combinations(len)
      } yield comb.mkString
      
    }
    def occurrencesToString(in: Occurrences): String = {
      (for {
	    elem <- in
	    i <- List.range(0, elem._2)
	  } yield elem._1).mkString
	}  
    (for (word <- combine(occurrencesToString(occurrences))) yield wordOccurrences(word)) 
    
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap
    val yMap = y.toMap
    
    (for (key <- xMap.keys) 
      yield (key, xMap.getOrElse(key, 0) - yMap.getOrElse(key, 0))
    ).toList.filter(e => (e._2 > 0)).sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   *  
   *  1. Build a tree:
   *     word1 word2
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    
    /*def getWordList(combinations: List[Occurrences]): List[Word] = {
      if (combinations.isEmpty) Nil
      else if (dictionaryByOccurrences.contains(combinations.head)) dictionaryByOccurrences.getOrElse(combinations.head, Nil)
      else getWordList(combinations.tail)
    }*/
    def getWordList(combinations: List[Occurrences]): List[Word] = {
      (for (occ <- combinations) yield dictionaryByOccurrences.getOrElse(occ, List[Word]())).flatten
    }
    
    def getWord(combinations: List[Occurrences]): Word = {
      val list = getWordList(combinations)
      if (list.isEmpty) ""
      else list.head
    }
    def sentenceAcc(sOccurrences: Occurrences, curSentence: Sentence, list: List[Sentence]): List[Sentence] = {
      if (sOccurrences.isEmpty) list ::: List[Sentence](curSentence)
      else if (getWordList(combinations(sOccurrences)).isEmpty) list
      else list ::: (for (word <- getWordList(combinations(sOccurrences))) yield sentenceAcc(subtract(sOccurrences, wordOccurrences(word)), curSentence ::: List(word), List[Sentence]())).flatten
    }   
    sentenceAcc(sentenceOccurrences(sentence), List[Word](), List[Sentence]())
    /*
    def sentenceListAcc(sOccurrences: Occurrences, occDictionary: Map[Occurrences, List[Word]], list: List[Sentence]): List[Sentence] = {
      def sentenceAcc(sOccurrences: Occurrences, list: List[Word]): List[Word] = {
        if (sOccurrences.length == 0) return list
        else for {
          combination <- combinations(sOccurrences)
          word <- occDictionary.getOrElse(combination, Nil)
        } yield sentenceAcc(subtract(sOccurrences, combination), list ::: List[Word](word))
        
      }
      if (sOccurrences.length == 0) {
        if (occDictionary.length == 0) return list
        else sentenceAcc(sentenceOccurrences(sentence), dictionary.tail, list)
      }
      else if (dictionary.length == 0) return list - list.last
      else if (dictionary.head not in combinations(sentenceOccurrences)) sentenceAcc(sOccurrences, dictionary.tail, list)
      else sentenceAcc(subtract(sOccurrences, wordOccurrences(dictionary.head), dictionary.tail, List[Sentence](list.head, list.tail.append(dictionary.head)))
    }
    sentenceListAcc(sentenceOccurrences(sentence), dictionaryByOccurrences, List[Sentence]())
    */
  }

}
