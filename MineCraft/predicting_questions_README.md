# Predicting Questions in Dialogue

A sample dialogue from the Minecraft Corpus:

> _(1) **Architect:** start with a stack of 5 purple blocks in the middle_
> 
> __(2) **Builder:** Now what?__
> 
> _(3) **Architect:** cool! extend the top purple block to a row of 5 purple blocks_
> 
> _(4) **Architect:** so like an upside down "L"__
> 
> __(5) **Builder:** So they should extend to one side, correct?__
> 
> _(6) **Architect:** yep!_
> 
> _(7) **Architect:** nice! now, put a block above and below the second block from the right_
> 
> __(8) **Builder:** What color should those blocks be?__

All three of the Builder's turns in this dialogue (lines 2, 5, and 8) are questions. They have some qualities in common: they all request something of the interlocutor, and they all signal their status as questions with a `question mark` at the end. This latter feature makes them easy to identify with one line of code: 
```r
minecraftcorpusdf %>% mutate(questionmark = grepl("\\?", text))
```
What causes people to ask questions in the Minecraft collaborative building task? Let's start by thinking about the Builder's questions from the excerpt above. 

The question on line 2, "Now what?" does not have an obvious antecedent - the Builder seems to have understood and excecuted the previous instruction and is now merely moving the conversation along by asking for another one.

The questions on lines 5 and 8, on the other hand, do have clear antecedents in the conversation. Specifically, they each refer to the architect's immediately preceding instruction and request clarification thereof. These questions can therefore be considered [repair initiations](https://onlinelibrary.wiley.com/doi/full/10.1111/tops.12339), turns in talk that identify trouble (i.e. a need for clarification) in a preceding turn or turns uttered by an interlocutor.

Hence the first theoretical answer to my question: What causes people to ask questions? The need for clarification.

Of course not all questions are repair initiations, and not all repair initiations are presented as questions. Nevertheless, the structure of the Minecraft collaborative building task makes the correlation very high. In the task, the Architect has access to _all_ of the information that the Builder needs to proceed (the design of the target structure) and information is the _only_ thing that the Builder can get from the Architect. This means that pretty much all of the Builder's questions are aimed at clarifying information coming from the Architect. 

For these reasons, and since I have no way of identifying true repair iniitations other than going through the whole corpus myself, I will operationally define `repair` as any Builder's utterance that includes a question mark.

```r
minecraftcorpusdf %>% mutate(repair = questionmark & role == "B")
```

