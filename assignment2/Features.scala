package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable
import scala.util.control._
/**
  * Created by Georgios on 05/11/2015.
  * @author: ying wen, wenqing wang, xinyu weng, yuege li
 */

object Features {
  val triggerWordSet = Set("controlled", "Producing", "stronger", "Protein", "association", "reexpression", "Maintained", "peak", "critical", "Without", "stabilizes", "Transgenic", "Inhibition", "expressions", "TG-induced", "-producing", "homodimerization", "present", "transcription", "suppressing", "-", "into", "-expression", "activation-induced", "alteration", "RESULTS", "Augmented", "highly", "responsible", "promote", "phosphotyrosine-containing", "unimpaired", "regulating", "interacting", "failed", "identified", "restored", "block", "induced", "regulation", "costimulation", "requirement", "detected", "inducibility", "determine", "Upregulation", "Rel/NF-kappaB-responsive", "gene-nonexpressing", "act", "inversion", "studies", "A-induced", "remained", "unchanged", "synthesized", "CsA-sensitive", "prevention", "modulate", "Cell-specific", "Control", "Increased", "lead", "deficient", "response", "constant", "Binding", "down-regulate", "couple", "depletion", "dominant-negative", "contributes", "subunit", "show", "transfectants", "resistance", "composed", "loss", "associates", "upregulates", "concentration", "producing", "pair", "accelerates", "costimulated", "sites", "acid-induced", "Phosphorylation", "Reactive", "decreased", "interacted", "attenuated", "active", "independent", "engagement", "occurring", "Phosphorylated", "suppression", "attenuates", "underlie", "control", "acts", "modulating", "forced", "regulator", ":", "abundance", "down-regulated", "without", "regulated", "promotion", "Transcription", "promoter", "formation", "reduction", "expression", "function", "linked", "under", "dimerized", "produce", "effect", "level", "imported", "hyperphosphorylated", "activity", "suppresses", "coproduce", "resulted", "augmented", "cis-acting", "cross-linked", "initiated", "essential", "mediated", "transgenic", "deficiency", "recruitment", "directing", "secreted", "enhancing", "heterodimers", "actions", "Role", "induce", "exclusive", "alters", "requires", "super-induced", "followed", "competition", "abrogated", "interfering", "effects", "Coexpression", "Upregulates", "effector", "responds", "EBV-mediated", "produced", "flow-induced", "-defective", "lowest", "site", "compensated", "When", "homodimers", "regulators", "receptor-induced", "inhibited", "reactivation", "positive", "Activation-dependent", "transfected", "neutralization", "occurs", "recognition", "made", "elicited", "Expression", "C5a-induced", "role", "production", "Enforced", "DNA-binding", "stimulating", "Additionally", "CNS-derived", "does", "cycloheximide-sensitive", "undetectable", "abrogation", "heterodimerizes", "Triggering", "deregulated", "controlling", "PMA-induced", "caused", "defect", "transcriptionally-active", "B-binding", "provided", "causing", "negative", "dysregulation", "revert", "established", "-Induced", "high", "favor", "-positve", "-Producing", "Involvement", "loop", "Increase", "detect", "oligomerization", "highest", "defective", "occupied", "decrease", "involve", "suppressed", "-independent", "altering", "lack", "core", "functional", "downregulated", "inactivation", "reduced", "restore", "-regulated", "inability", "heterodimerize", "B-dependent", "inducing", "Cotransfection", "inhibit", "presence", "coproducing", "unaffected", "found", "autoinduced", "factors", "Transgene", "down-regulates", "transactivation", "repressing", "down-regulating", "re-expression", "Following", "enhance", "To", "upon", "product", "coexpressed", "repression", "up-regulated", "interaction", "signaling", "activate", "increasing", "abolished", "at", "repressor", "associate", "reverses", "maintained", "blocks", "phosphorylated", "localization", "required", "localize", "deletion", "lower", "immunoreactivity", "enzyme", "inducible", "cooperative", "transfection", "that", "-deficient", "conditional", "are", "interact", "targets", "blocked", "masks", "causes", "source", "Downregulation", "release", "cytokine-induced", "mobilized", "susceptible", "confers", "transactivated", "ligation", "cooperate", "prevented", "express", "changes", "myristate-induced", "from", "Repression", "engage", "rise", "Consequently", "study", "affect", "culminating", "Induction", "responsive", "migrating", "heterodimer", "TPA-induced", "sequence", "responses", "RNA", "abnormal", "sufficient", "result", "activities", "upstream", "complex", "engaged", "absent", "abolish", "activated", "detectable", "augmenting", "increases", "-nonproducing", "when", "enhances", "phosphorylation", "CoCl2-induced", "secreting", "not", "important", "lectins", "signals", "lipopolysaccharide-induced", "recognized", "downregulate", "Excess", "deregulation", "trigger", "Regulation", "PMA/Iono-stimulated", "self-perpetuation", "Down-regulation", "using", "comigrated", "LPS-induced", "signal", "Overexpression", "knock-out", "involves", "coimmunoprecipitated", "hindrance", "retargeting", "depending", "up-regulates", "upregulated", "introducing", "inactivates", "involvement", "inducer", "Lipopolysaccharide-induced", "conditions", "dependence", "decline", "1-reverse", "transcriptionally", "promotes", "lost", "mechanism", "IFN-alpha-induced", "after", "showed", "protein", "ligand-induced", "through", "NF-kappaB-dependent", "transcribed", "determination", "mechanisms", "stabilizing", "provides", "neutralizing", "supported", "lipopolysaccharide-stimulated", "occurred", "Coligation", "used", "associating", "resulting", "led", "ability", "controls", "activation", "binds", "blocking", "Requires", "shear-induced", "depends", "observed", "LPS-Induced", "rescue", "stimulus-induced", "After", "underlies", "site-dependent", "interference", "promoters", "concentrations", "leads", "coproduced", "inhibitors", "mediate", "mobilization", "Stimulation", "Activation", "immunoprecipitated", "reduces", "enhancer", "restricted", "secretion", "proteins", "activators", "augments", "altered", "abundantly", "affected", "participation", "down-regulation", "impaired", "up-regulate", "correlation", "stability", "retarget", "factor", "acted", "increased", "cross-linking", "proportion", "consequence", "silencing", "augment", "up-regulation", "Changes", "expresses", "addition", "modulated", "directs", "inhibition", "disrupts", "exported", "coactivator", "rescued", "staining", "interacts", "represses", "amounts", "Targeted", "failure", "downregulates", "regulate", "make", "cotransfection", "Translocation", "downregulation", "abrogating", "Altered", "resynthesis", "strong", "alter", "accumulation", "dimerizing", "responsiveness", "low", "intense", "stimulates", "maintenance", "targeted", "due", "superinduction", "mediating", "precipitable", "target", "co-transfections", "Signaling", "-inducible", "tyrosine-phosphorylated", "superinduced", "B-cell-specific", "removed", "necessary", "-stimulated", "selection", "transactivate", "turning", "-expressing", "-mediated", "TLR-dependent", "interfacing", "sensitive", "super-induction", "functions", "unaltered", "recognize", "reducing", "via", "genotype", "inhibiting", "associated", "recruit", "increase", "induces", "coligation", "Absence", "coexpressing", "interactions", "vary", "co-expressed", "cotransfecting", "inactivated", "appearance", "Stability", "during", "overexpressed", "inhibitory", "repress", "specific-transcription", "prevent", "degraded", "recognizes", "derived", "KB-enhancer", "Production", "mRNA", "Upon", "Activated", "proteolysis-resistant", "following", "pathways", "higher", "elevated", "preventing", "lose", "replaced", "expressed", "accelerating", "capacity", "recruits", "growth-regulated", "transfer", "forms", "stabilization", "involved", "need", "With", "diminished", "inhibitor", "TCR-mediated", "confer", "deprivation", "defining", "prolonged", "enhanced", "weakly", "affecting", "upregulation", "lacking", "Increases", "-induced", "process", "depend", "squelch", "affinity", "sustains", "conferring", "silence", "reverted", "-negative", "reactive", "triggering", "immobilization", "bind", "IFN-induced", "initiates", "pathway", "depended", "absence", "obtained", "self-perpetuating", "proteasome-mediated", "stimulation", "abolishes", "participate", "drive", "inhibits", "Cross-linking", "localizes", "transcriptional", "correlated", "demonstrated", "stabilized", "Deregulation", "Transcriptional", "contribute", "downstream", "High", "-catalyzed", "translocation", "element", "binding", "Dependent", "identifier", "by", "coupled", "+", "receptor", "number", "eliminates", "leading", "expressing", "degrade", "released", "induction", "synthesis", "Levels", "of", "PG490", "dependent", "activates", "Reduction", "sustaining", "From", "repressed", "coexpression", "-positive", "enhancement", "with", "restriction", "more", "mutants", "Contribution", "high-level", "regulatory", "reacted", "affects", "define", "localized", "overexpression", "levels", "demonstrate", "destabilization", "results", "stimuli", "Neutralizing", "requirements", "promoting", "targeting", "synergize", "observations", "for", "Interaction", "-dependent", "transcripts", "stimulated", "decreases", "accumulates", "influence", "associations", "limited", "require", "complexes", "account", "implicated", "assembly", "mediates", "alterations", "amplified", "underphosphorylated", "downmodulation", "to", "activator", "sustained", "stimulate", "regulates", "Activates", "preformed", "post-transcription", "action", "Deletion", "permits", "degradation", "abrogates", "upregulate", "Disruption", "dex-resistant", "generated", "bound")



  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Trigger Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats.toMap
  }
  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Argument Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)

    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument

    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0
    feats.toMap
  }



  def p3TriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey, Double]
    /******1******/
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    /******2******/
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature

    /*3*/
    //feats += FeatureKey("first 4 characters of first trigger word", List(token.word.toCharArray.slice(0,4).toString, y)) -> 1.0

    /*4*/
    feats += FeatureKey("trigger_end_word", List(thisSentence.tokens(end).word, y)) -> 1.0


    /******5******/
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString, token.word, y)) -> 1.0
    /******6******/
    //feats += FeatureKey("number of arguments", List(x.arguments.size.toString, y)) -> 1.0 //word feature

    /******7******/
    //feats += FeatureKey("number of events", List(thisSentence.events.size.toString,token.word, y)) -> 1.0 //word feature
    /******8******/
    //feats += FeatureKey("POS tag of trigger_end_word", List(thisSentence.tokens(end).pos, y)) -> 1.0 //word feature

    /******9******/
    //bigtams of the current and cuntext words within the window of size 2
    feats += FeatureKey("bigram trigger word", NgramHistory(thisSentence, begin, 2) ::: List(token.word, y)) -> 1.0
    /******10******/
    //dependency types associated the current token
    for (i <- thisSentence.deps) {
      if (i.head == begin) {
        val modword = thisSentence.tokens(i.mod).word
        feats += FeatureKey("Dependency Type between trigger word and mod word", List(i.label, modword, token.word, y)) -> 1.0
      }
      /******11******/
      if (i.mod == begin) {
        val headword = thisSentence.tokens(i.head).word
        feats += FeatureKey("Dependency Type between trigger word and head word", List(i.label, headword, token.word, y)) -> 1.0
      }
    } //good with ngram trigger word

    /******12******/
    var menCount = 0
    feats += FeatureKey("number of mentions in the sentence", List(thisSentence.mentions.size.toString, y)) -> 1.0
    thisSentence.mentions.foreach(m => {
      if(m.begin <= (begin + 3) || m.begin >= (begin-3))
        menCount += 1
      /******13******/
      feats += FeatureKey("content of mentions", thisSentence.tokens.slice(m.begin, m.end).map(tt => tt.word).toList ::: List(y)) -> 1.0
    })

    /******14******/
    //feats += FeatureKey("number of mentions around with the window of 3", List(menCount.toString,y))-> 1.0

    feats.toMap
  }

  def p5TriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey, Double]
    // 1. bias feature
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    // 2. first trigger word
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
//    feats += FeatureKey("first stem word", List(token.stem, y)) -> 1.0 //word feature
//    feats += FeatureKey("first pos word", List(token.pos, y)) -> 1.0 //word feature
    // 3. is protein_first trigger word
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString, token.word, y)) -> 1.0

    // 4. argument num
    feats += FeatureKey("argument num", List(x.arguments.size.toString, y)) -> 1.0 //word feature

    //bigtams of the current and cuntext words within the window of size 2

    // 5. bigram trigger word
    feats += FeatureKey("bigram trigger word", NgramHistory(thisSentence, begin, 2) ::: List(token.word, y)) -> 1.0
//    feats += FeatureKey("trigram trigger word", NgramHistory(thisSentence, begin, 3) ::: List(token.word, y)) -> 1.0

    // 6. dependency types associated the current token
    for (i <- thisSentence.deps) {
      if (i.head == begin) {
        val modword = thisSentence.tokens(i.mod).word
        feats += FeatureKey("Dependency Type of trigger word - mod", List(i.label, modword, token.word, y)) -> 1.0
      }
      if (i.mod == begin) {
        val headword = thisSentence.tokens(i.head).word
        feats += FeatureKey("Dependency Type of trigger word - head", List(i.label, headword, token.word, y)) -> 1.0
      }
    } //good with ngram trigger word

    // 7. mentions related
    var menCount = 0
    feats += FeatureKey("mention num", List(thisSentence.mentions.size.toString, y)) -> 1.0
    thisSentence.mentions.foreach(m => {
      if(m.begin <= (begin + 3) || m.begin >= (begin-3))
        menCount += 1
      feats += FeatureKey("mentions", thisSentence.tokens.slice(m.begin, m.end).map(tt => tt.word).toList ::: List(y)) -> 1.0
//      feats += FeatureKey("mentions",  List(thisSentence.tokens(m.begin).word,y)) -> 1.0

    })
    feats += FeatureKey("mention num around with window 3", List(menCount.toString,y))-> 1.0



    if(begin == 0 )
      feats += FeatureKey("index of trigger word", List(begin.toString,token.word, y)) -> 1.0



    // 8.if the word in trigger word set
    feats += FeatureKey("if in trigger word set", List(triggerWordSet.contains(token.word).toString,token.word,y))-> 1.0


    feats.toMap
  }

  def myTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey, Double]
    /******1******/
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    /******2******/
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature

    /*3*/
    //feats += FeatureKey("first 4 characters of first trigger word", List(token.word.toCharArray.slice(0,4).toString, y)) -> 1.0

    /*4*/
    feats += FeatureKey("trigger_end_word", List(thisSentence.tokens(end).word, y)) -> 1.0


    /******5******/
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString, token.word, y)) -> 1.0
    /******6******/
    //feats += FeatureKey("number of arguments", List(x.arguments.size.toString, y)) -> 1.0 //word feature

    /******7******/
    //feats += FeatureKey("number of events", List(thisSentence.events.size.toString,token.word, y)) -> 1.0 //word feature
    /******8******/
    //feats += FeatureKey("POS tag of trigger_end_word", List(thisSentence.tokens(end).pos, y)) -> 1.0 //word feature

    /******9******/
    //bigtams of the current and cuntext words within the window of size 2
    feats += FeatureKey("bigram trigger word", NgramHistory(thisSentence, begin, 2) ::: List(token.word, y)) -> 1.0
    feats += FeatureKey("bigram trigger word", NgramHistory(thisSentence, begin, 3) ::: List(token.word, y)) -> 1.0
    /******10******/
    //dependency types associated the current token
    for (i <- thisSentence.deps) {
      if (i.head == begin) {
        val modword = thisSentence.tokens(i.mod).word
        feats += FeatureKey("Dependency Type between trigger word and mod word", List(i.label, modword, token.word, y)) -> 1.0
      }
      /******11******/
      if (i.mod == begin) {
        val headword = thisSentence.tokens(i.head).word
        feats += FeatureKey("Dependency Type between trigger word and head word", List(i.label, headword, token.word, y)) -> 1.0
      }
    } //good with ngram trigger word

    /******12******/
    var menCount = 0
    feats += FeatureKey("number of mentions in the sentence", List(thisSentence.mentions.size.toString, y)) -> 1.0
    thisSentence.mentions.foreach(m => {
      if(m.begin <= (begin + 3) || m.begin >= (begin-3))
        menCount += 1
      /******13******/
//      feats += FeatureKey("content of mentions", thisSentence.tokens.slice(m.begin, m.end).map(tt => tt.word).toList ::: List(y)) -> 1.0
    })

    /******14******/
//    feats += FeatureKey("number of mentions around with the window of 3", List(menCount.toString,y))-> 1.0

    if(begin == 0 )
      feats += FeatureKey("index of trigger word", List(begin.toString,token.word, y)) -> 1.0



    // 8.if the word in trigger word set
    feats += FeatureKey("if in trigger word set", List(triggerWordSet.contains(token.word).toString,token.word,y))-> 1.0

    feats.toMap

  }


  def myArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey, Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString, eventHeadToken.word, y)) -> 1.0
    val dependency = thisSentence.deps

    val word = token.word

    if(begin>0) {
      val formerToken=thisSentence.tokens(begin - 1)
      feats += FeatureKey("bigram stem argument", List(formerToken.stem, token.word, y)) -> 1.0
    }
    feats += FeatureKey("mention", List(x.isProtein.toString, y)) -> 1.0
    feats += FeatureKey("event word", List(thisSentence.tokens(event.begin).word,y)) -> 1.0 //!!
    feats += FeatureKey("event pos", List(thisSentence.tokens(event.begin).pos,y)) -> 1.0 //!!

    val mentions = thisSentence.mentions.find(m=>m.begin==begin)
    if(mentions.size > 0){
      val mention_label = mentions.get.label
      feats += FeatureKey("mention_label", List(mention_label,word, y)) -> 1.0
    }
//    feats += FeatureKey("mentions num", List(thisSentence.mentions.length.toString,word, y)) -> 1.0

    feats += FeatureKey("trigger word", List(eventHeadToken.word, y)) -> 1.0
    //WWQ

    //length of argument
    val length=end-begin
    feats += FeatureKey("length of argument", List(length.toString, token.word, y)) -> 1.0

    //dependency label
    for(dep<-thisSentence.deps){
      if(dep.mod==begin){
        feats += FeatureKey("dependency label", List(dep.label, y)) -> 1.0
      }
    }


    //context words of entity mention
    thisSentence.mentions.foreach(e=>{
      feats += FeatureKey("mention label", List(e.label, y)) -> 1.0
      //      feats += FeatureKey("mention words", thisSentence.tokens.slice(e.begin,e.end).map(tt=>tt.word).toList ::: List(y))-> 1.0
    })


    //dependency on trigger
    //    for(dep<-thisSentence.deps){
    //      if(dep.mod==begin){
    //        feats += FeatureKey("dependency label", List(dep.label, y)) -> 1.0
    //      }
    //    }



    feats += FeatureKey("distance from trigger to argument", List(math.abs(begin-event.begin).toString,y)) -> 1.0

    for(i<-thisSentence.deps){
      if(i.head==begin){
        val modword=thisSentence.tokens(i.mod).word
        feats += FeatureKey("Dependency Type of argument word - mod", List(i.label,modword,token.word,y))-> 1.0
      }
      if(i.mod==begin){
        val headword=thisSentence.tokens(i.head).word
        feats += FeatureKey("Dependency Type of argument word - head", List(i.label,headword,token.word, y))-> 1.0
      }


    }


    val mods = mutable.Map[Integer, Integer]() withDefaultValue 0
    val heads = mutable.Map[Integer, Integer]() withDefaultValue 0
    dependency.foreach(d => {
      mods(d.mod) += 1
      heads(d.head) += 1
    })

    //the mod number of event
    feats += FeatureKey("number mod and head of event", List((mods(event.begin)+heads(event.begin)).toString, y)) -> 1.0

    //和event之间deps的label
    dependency.foreach(d => {
      if(d.mod == begin && d.head == event.begin)
        feats += FeatureKey("deps' label between event and argument", List(d.label, y)) -> 1.0
    })

    //event的词干
    feats += FeatureKey("event stem", List(thisSentence.tokens(event.begin).stem, y)) -> 1.0

    //词干，大写数字，protein
    feats += FeatureKey("!!!!!", List(token.stem, word.toUpperCase().equals(word).toString, x.isProtein.toString, y)) -> 1.0

    //存在符号
    feats += FeatureKey("Punctuation", List(existPunc(word).toString,thisSentence.tokens(end).pos, y)) -> 1.0

    //后一个词内容
    feats += FeatureKey("following", List(thisSentence.tokens(end).word, y)) -> 1.0 //word feature
//    feats += FeatureKey("following", List(thisSentence.tokens(end).pos, y)) -> 1.0 //word feature

    feats.toMap

  }

  def p3ArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey, Double]
    feats += FeatureKey("argument label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    val word = token.word


    /** related to argument word*/
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0

    if(begin>0) {
      val lastToken=thisSentence.tokens(begin - 1)
      feats += FeatureKey("bigram argument stem", List(lastToken.stem, token.word, y)) -> 1.0
    }

    //next word
    feats += FeatureKey("next word", List(thisSentence.tokens(end).word, y)) -> 1.0 //word feature

    //trigram
    feats += FeatureKey("trigram argument word", NgramHistoryArg(thisSentence,begin,3):::List(token.word, y)) -> 1.0




    /** related to POS or dependency */
    if(begin>0) {
      val lastToken=thisSentence.tokens(begin - 1)
      feats+=FeatureKey("POS tags of last token and first_argument_word", List(lastToken.pos, token.pos, y))->1.0
    }

    //pos equals NN
    val thisPOS=token.pos
    val nextPOS=thisSentence.tokens(end).pos
    if(thisPOS=="NN"){
      feats += FeatureKey("POS tag next to the argument_word tagged as NN", List(nextPOS, y)) -> 1.0
    }

    for(i<-thisSentence.deps){
      if(i.head==begin){//arg is head
      val modword=thisSentence.tokens(i.mod).word
        feats += FeatureKey("Dependency Type between argument word and mod word", List(i.label,modword,token.word,y))-> 1.0
      }
      if(i.mod==begin){//arg is mod
      val headword=thisSentence.tokens(i.head).word
        feats += FeatureKey("Dependency Type between argument word and head word", List(i.label,headword,token.word, y))-> 1.0
        feats += FeatureKey("dependency label", List(i.label, y)) -> 1.0
      }
    }

    //argument contains punctuation, pos of next token
    // feats += FeatureKey("Punctuation of arg combined with pos of next token", List(existPunc(word).toString,thisSentence.tokens(end).pos, y)) -> 1.0


    /** related to mentions*/
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString, eventHeadToken.word, y)) -> 1.0
    feats += FeatureKey("isProtein_fist word", List(x.isProtein.toString, y)) -> 1.0

    //label of entity mention in the sentence
    thisSentence.mentions.foreach(e=>{
      feats += FeatureKey("mention label", List(e.label, y)) -> 1.0
      //       words of entity mention
      feats += FeatureKey("words of entity mention", thisSentence.tokens.slice(e.begin,e.end).map(tt=>tt.word).toList ::: List(y))-> 1.0
    })

    //stem，Capital，is_protein
    feats += FeatureKey("containing stem，Capitalized and is_protein", List(token.stem, word.toUpperCase().equals(word).toString, x.isProtein.toString, y)) -> 1.0

    //Ngram pos
    feats += FeatureKey("trigram pos", AroundPos(thisSentence,begin,3):::List(token.pos, y)) -> 1.0


    /** related to event*/
    //dependency on trigger word
    feats += FeatureKey("dependency on trigger word", List(eventHeadToken.word, word, y)) -> 1.0

    feats += FeatureKey("number of events + first argument word", List(thisSentence.events.size.toString,word, y)) -> 1.0

    //stem of trigger
    feats += FeatureKey("trigger stem", List(eventHeadToken.stem, y)) -> 1.0

    //pos of trigger
    feats += FeatureKey("trigger pos", List(eventHeadToken.pos, y)) -> 1.0


    var minDistArgBegin=0
    var minDist=100
    if(event.arguments.size>1) {
      event.arguments.foreach(arg => {
        val distance = math.abs(arg.begin - begin)
        if (arg.begin != begin && distance < minDist) {
          minDistArgBegin = arg.begin
          minDist = distance
        }
      })
      //nearest arg word
      feats += FeatureKey("nearest argument word", List(thisSentence.tokens(minDistArgBegin).word,word, y)) -> 1.0
      //min distance from other arguments
      feats += FeatureKey("min distance from other arguments", List(minDist.toString,word, y)) -> 1.0
      //nearest arg stem??
      feats += FeatureKey("nearest argument stem", List(thisSentence.tokens(minDistArgBegin).stem,word, y)) -> 1.0
      //nearest arg pos??
      feats += FeatureKey("nearest argument pos", List(thisSentence.tokens(minDistArgBegin).pos,token.pos, y)) -> 1.0
    }



    /** related to numbers, length, distance*/
    //length of arguments and context of the word
    val length=end-begin
    feats += FeatureKey("length of argument and first_argument_word", List(length.toString, token.word, y)) -> 1.0

    //distance from trigger
    feats += FeatureKey("distance from trigger to argument", List(math.abs(begin-event.begin).toString,y)) -> 1.0

    //relative position of argument to trigger
    var position="overlap"
    if(begin<event.begin) position="before" else position="after"
    feats += FeatureKey("relative position of argument to trigger", List(position,y)) -> 1.0

    //number of mods and heads of argument
    val mods = mutable.Map[Integer, Integer]() withDefaultValue 0
    val heads = mutable.Map[Integer, Integer]() withDefaultValue 0
    thisSentence.deps.foreach(d => {
      mods(d.mod) += 1
      heads(d.head) += 1
    })
    feats += FeatureKey("number of mods and heads of argument", List((mods(event.begin)+heads(event.begin)).toString, y)) -> 1.0

    feats.toMap

  }

  def p5ArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey, Double]

    // 1. label bias
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    // 2. first argument word
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    // 3. is protein_first trigger word
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString, eventHeadToken.word, y)) -> 1.0



    val dependency = thisSentence.deps
    val word = token.word
    val words = thisSentence.tokens.slice(begin,end).map(tt=>tt.word).mkString(" ")
//    val stems = thisSentence.tokens.slice(begin,end).map(tt=>tt.stem).mkString(" ")
    val poss = thisSentence.tokens.slice(begin,end).map(tt=>tt.pos).mkString(" ")

    // 4. bigram trigram of arugument token
    feats += FeatureKey("bigram trigger word", NgramHistory(thisSentence, begin, 2) ::: List(token.word, y)) -> 1.0
    feats += FeatureKey("trigram trigger word", NgramHistory(thisSentence, begin, 3) ::: List(token.word, y)) -> 1.0

    // 5. whole words
    feats += FeatureKey("whole argument words", List(words, y)) -> 1.0
//    feats += FeatureKey("whole argument stems", List(stems, y)) -> 1.0
//    feats += FeatureKey("whole argument poss", List(poss, y)) -> 1.0

    // 6. digit/punc/captain
    feats += FeatureKey("if contain digit", List(existNum(words).toString, y)) -> 1.0
    feats += FeatureKey("if contain punc", List(existPunc(words).toString, y)) -> 1.0
    feats += FeatureKey("if contain uppercase", List(words.toUpperCase().equals(words).toString, y)) -> 1.0


    // 7. if the first word of sentence
    if(begin == 0)
      feats += FeatureKey("argument as first word", List("true", y)) -> 1.0

    // 8. dependency of argument
    dependency.foreach(d => {
      if(d.mod == begin && d.head == event.begin)
        feats += FeatureKey("dependency label between event and argument", List(d.label, y)) -> 1.0
      else{
        if(d.head==begin){
          val modword=thisSentence.tokens(d.mod)
          feats += FeatureKey("Dependency Type of argument word - mod", List(d.label,modword.word,y))-> 1.0
        }
        if(d.mod==begin){
          val headword=thisSentence.tokens(d.head)
          feats += FeatureKey("Dependency Type of argument word - head", List(d.label,headword.word, y))-> 1.0
        }
      }
    })

    // 9. following word
    feats += FeatureKey("following word", List(thisSentence.tokens(end).word,word,y)) -> 1.0 //word feature


    feats.toMap
  }

  def NBArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey, Double]
    feats += FeatureKey("argument label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    val word = token.word

    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    for(i<-thisSentence.deps){

      if(i.mod==begin){//arg is mod
      val headword=thisSentence.tokens(i.head).word
        feats += FeatureKey("dependency label", List(i.label, y)) -> 1.0
      }
    }
    feats += FeatureKey("isProtein_fist word", List(x.isProtein.toString, y)) -> 1.0

    feats += FeatureKey("distance from trigger to argument", List(math.abs(begin-event.begin).toString,y)) -> 1.0
    val thisPOS=token.pos
    val nextPOS=thisSentence.tokens(end).pos
    if(thisPOS=="NN"){
      feats += FeatureKey("POS tag next to the argument_word tagged as NN", List(nextPOS, y)) -> 1.0
    }
    val mods = mutable.Map[Integer, Integer]() withDefaultValue 0
    val heads = mutable.Map[Integer, Integer]() withDefaultValue 0
    thisSentence.deps.foreach(d => {
      mods(d.mod) += 1
      heads(d.head) += 1
    })
    feats += FeatureKey("number of mods and heads of argument", List((mods(event.begin)+heads(event.begin)).toString, y)) -> 1.0
    feats += FeatureKey("number of mods and heads of argument", List((mods(event.begin)+heads(event.begin)).toString, (mods(begin)+heads(begin)).toString, y)) -> 1.0


    feats.toMap

  }

  def NBTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey, Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    var menCount = 0
    feats += FeatureKey("mention num", List(thisSentence.mentions.size.toString, y)) -> 1.0
    feats.toMap
  }

  def existPunc(word: String): Boolean = {
    var b = false
    for (i <- 0 until word.length) {
      if (word(i).equals("-") || word(i).equals("\\") || word(i).equals(",") || word(i).equals(".") || word(i).equals("(")|| word(i).equals(")")) {
        b = true
      }
    }
    b
  }

  def existNum(word: String): Boolean = {
    var b = false
    for (i <- 0 until word.length) {
      if (word(i).isDigit) {
        b = true
      }
    }
    b
  }

  def NgramHistory(thisSentence: Sentence, begin: Int, order: Int):List[String]={
    //var history:List[String]=Nil
    //val history = ListBuffer[String]
    var history = mutable.MutableList[String]()
    if(begin>order-1){
      //for(i<-1 to order-1) thisSentence.tokens(begin-i).word += history
      for(i<-1 to order-1)  history += thisSentence.tokens(begin-order+i).word
    }else{
      for(i<-1 to order-begin-1) history += "*"
      for(i<-1 to begin) history += thisSentence.tokens(begin-order+i+1).word
      //add "*" to history when the number of words before the token is smaller than order-1
    }
    history.toList
  }

  def NgramHistoryArg(thisSentence: Sentence, begin: Int, order: Int):List[String]={
    var history = mutable.MutableList[String]()
    if(begin>order-1){
      for(i<-begin-order+1 to begin-1)  history += thisSentence.tokens(i).word
    }else{
      for(i<-begin-order+1 until 0) history += "*"
      for(i<-0 to begin-1) history += thisSentence.tokens(i).word
      //add "*" to history when the number of words before the token is smaller than order-1
    }
    history.toList
  }

  def AroundPos(thisSentence: Sentence, begin: Int, n: Int):List[String]={
    var aroundPos = mutable.MutableList[String]()
    if(begin>n-1){
      for(i<-begin-n+1 to begin-1) aroundPos+=thisSentence.tokens(i).pos
    }else{
      for(i<-0 to begin-1) aroundPos+=thisSentence.tokens(begin-i).pos
      for(i<-begin-n+1 until 0) aroundPos+="*"
      //add "*" to history when the number of words before the token is smaller than order-1
    }
    aroundPos.toList
  }




}
