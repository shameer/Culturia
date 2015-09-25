# Notes

Notes about [CogPrime Overview](http://wiki.opencog.org/wikihome/index.php/CogPrime_Overview)

*Large sentence in italic means more or less that I don't understand*

Notes taking algorithm:

- global: keep the intent ie. overview of opencog with research grounds
  - rework the document hierarchy to make more sens
  - keep the research background but make it less proeminent

- local: simplify local cognitive load, the general rule is to avoid creating or
  opening doors and focus and what are the directions and make easier for
  contribution from a wide range of people:

  - simplify wording aka. avoid words I don't know. I'm sorry Yeats.
  - simplify phrasing aka. less prose more bullets
  - simplify actual state of the scientific advancement aka. do not text link to
    non-wikipedia articles.
  - avoid adjectives that provide no value for direct understanding of opencog
  project intent but provide scientifical background.
  - less of “because X, Y we should Z”  and instead “we must Z” 
  - less contextualisation

- glocal
  - Try to sort out what is opencog from what is opencog prime specific.
  Everything that has not source repository is OpenCogPrime.
  - Do not cite thing with no prior explanation (except glocal)
  - strip examples

## Introduction

### AI vs AGI

AI is really narrow-AI. AGI also know as *Human Level Intelligence* is a revival
of original AI goals to create a human-like intelligence.

### What's the secret sauce

There is no particular new algorithm or architecture principle [except PLN which
it's based on previous work].

Typically, the AI algorithms and structures corresponding to different cognitive
functions have been developed based on divergent background theories. They have
been tuned for effective performance on different narrow AI tasks in different
environments. **Making such diverse components work together** in a *synergetic*
and *cooperative* way is the “secret sauce”.

- *synergy*: the interaction of elements that when combined produce a total
  effect that is greater than the sum of the individual elements, contributions,
  etc.

### What Kind of “Intelligence” is CogPrime Aimed At?

This is defined by pratical goals. While these are not useful for motivating the
early steps of AGI research, we intend that a fully realized CogPrime system
would be able to succeed at them:

#. *Turing Test*: the classic, involving passing as a human being in an everyday
   conversation
#. *Virtual World* or *Telerobotic Turing Test* : pass as a human being, in a
   conversation involving controlling a virtual world avatar or a robot,
   alongside verbal interaction 
#. *Online University Test*: attend an online university just like a human
   student, and graduate
#. *Physical University Test*: control a robot that attends university just
   like a human student, and graduates
#. *Artificial Scientist Test*: write and publish original science papers,
   based on ideas conceived by the AI due to its own reading of the literature 

Another important aspect of intelligence is *adaptiveness*: the capability of a
system to adapt to its environment [recognize similar problems and apply
previous structure and algorithm] effectively.

[There is also an argument about “ressources” and “limited ressources”.
I'll paraphrase  Alan Kay argument “To build the software of the future
use the ressources of the future”
[https://www.youtube.com/watch?v=gTAghAJcO1o](The Future doesn't have to be incremental)]

CogPrime is designed toward human-relevant goals and environment. I is also
expected to have a particular set of intellectual strengths and weaknesses
different from that of humans.

### Key Claims

This is a list of claims that the reader must accept to validate that CogPrime
approach to AGI is a viable one.

An AGI system must:

#. Use much of its resources
#. Represent different kind of knowledge via different kinds of memory: declarative, procedural, episodic, sensory,
   intentional, attentional. 
#. Use cognitive synergy between different cognitive process to overcome
   processing bottlenecks. This means that cognitive processus must be able to
   understand and use the memory of each other.
#. have a global, local and *glocal* memory
#. have sensory data and motoric affordances that roughly emulate those
   available to humans.
#. must grow/evolve up along a path roughly comparable to that followed by human children. It is not human intelligence man, it learns human intelligence.
#. learning must happend using a mix of spontaneous learning and explicit instricution:
   - imitation
   - reinforcement and correction
   - linguistic and nonlinguistic instruction
#. learn human language via built-in NLP facility that can be improved based on
   experiences. NLP facility is both rules and statistic based
#. recognize and represent large-scale pattern in itself called the *cognitive equation*
   [reference: Chaotic Logic. Plenum, 1994. BibTeX [Goe94]].
#. upon sustained interaction with an environment in pursuit of goals emerge
   internal knowledge network, including but not limited to:
   - *hierarchical network: representing both a spatiotemporal hierarchy and an
     approximate “default inheritance” hierarchy, cross-linked*
   - [heterarchical network](https://en.wikipedia.org/wiki/Heterarchy) *of
     associativity, roughly aligned with the hierarchical network*
   - *self network which is an approximate micro image of the whole network*
   - *inter-reflecting networks modeling self and others, reflecting a “mirrorhouse” design pattern* [reference: Mirror Neurons, Mirrorhouses, and the Algebraic Structure of the Self GASP08].
#. implement implement a simplicity bias in each cognitive process cf. [Occam’s Razor](https://en.wikipedia.org/wiki/Occam's_razor).
#. if supplied with a commonsensically ethical goal system and an intentional
   component based on rigorous uncertain inference, should be able to reliably
   achieve a much higher level of commonsensically ethical behavior than any
   human being.
#. once sufficiently advanced, an AGI system with a logic-based declarative
   knowledge approach and a program-learning-based procedural knowledge approach
   should be able to radically self-improve via a variety of methods, including
   supercompilation and automated theorem-proving. 

[In the original article this was a point in the above list, this goes a bit
further in the definition of what an AGI must do and instead dive into how
it should do it, describing all the components of the OpenCog]

Given the strengths and weaknesses of current and near-future digital computers:

- *a (loosely) neural-symbolic network is a good representation for directly storing many kinds of memory, and interfacing between those that it doesn’t store directly;*
- *Uncertain logic is a good way to handle declarative knowledge. To deal with the problems facing a human-level AGI, an uncertain logic must integrate imprecise probability and fuzziness with a broad scope of logical constructs. [PLN](http://wiki.opencog.org/wikihome/index.php/Probabilistic_Logic_Networks) is one good realization.*
- *Programs are a good way to represent procedures (both cognitive and physical-action, but perhaps not including low-level motor-control procedures).*
- *Evolutionary program learning is a good way to handle difficult program learning problems. Probabilistic learning on normalized programs is one effective approach to evolutionary program learning. [MOSES](http://wiki.opencog.org/wikihome/index.php/Meta-Optimizing_Semantic_Evolutionary_Search) is one good realization of this approach.*
- Multistart hill-climbing, with a strong Occam prior, is a good way to handle relatively straightforward program learning problems.
- Activation spreading and Hebbian learning comprise a reasonable way to handle attentional knowledge (though other approaches, with greater overhead cost, may provide better accuracy and may be appropriate in some situations).
  - Artificial economics is an effective approach to activation spreading and Hebbian learning in the context of neural-symbolic networks; 
  - ECAN is one good realization of artificial economics; 
  - A good trade-off between comprehensiveness and efficiency is to focus on two kinds of attention: processor attention (represented in CogPrime by ShortTermImportance) and memory attention (represented in CogPrime by LongTermImportance).
- Simulation is a good way to handle episodic knowledge (remembered and imagined). Running an internal world simulation engine is an effective way to handle simulation. 
- Hybridization of one’s integrative neural-symbolic system with a spatiotemporally hierarchical deep learning system is an effective way to handle representation and learning of low-level sensorimotor knowledge. DeSTIN is one example of a deep learning system of this nature that can be effective in this context. 
- One effective way to handle goals is to represent them declaratively, and allocate attention among them economically. CogPrime ’s PLN/ECAN based framework for handling intentional knowledge is one good realization.

[Missing reference to [Relex](http://wiki.opencog.org/wikihome/index.php/RelEx_Dependency_Relationship_Extractor) and GrammarLink]

## CogPrime and OpenCog

- OpenCog is framework
- CogPrime is an AGI
- OpenCogPrime is the implementation of CogPrime using OpenCog

### Applications

- PLN + Relex: automated biological hypothesis generation based on information
  gathered from PubMed abstracts
- MOSES component for biological data analysis;
- financial prediction, genetics, marketing data analysis and natural language
  processing.

Most relevant: OpenCog has also been used to control agents in virtual worlds

#### Virtual Agents


[OpenPetBrain](deadlink://) control virtual dog in a virtual world.

Functionalities includes:

- learning new behaviors based on imitation and reinforcement
- responding to natural language commands and questions, with appropriate
  actions and natural language replies
- spontaneous exploration of their world, remembering their experiences and
  using them to bias future learning and linguistic interaction

Other tasks that could be implemented:

- Learning to build steps or ladders to get desired objects that are high up
- Learning to build a shelter to protect itself from aggressors
- Learning to build structures resembling structures that it's shown
  (even if the available materials are a bit different)
- Learning how to build bridges to cross chasms

the AI significance of learning tasks like this depends on:

- what kind of feedback the system is given,
- how complex its environment is

The goal is to have the system learn to carry out tasks like this using general
learning mechanisms and a general cognitive architecture, based on embodied
experience and only scant feedback from human teachers.

Tasks of the project *at time of writing* include:

- Watch another character build steps to reach a high-up object
- Figure out via imitation of this that, in a different context, building steps
  to reach a high up object may be a good idea
- Also figure out that, if it wants a certain high-up object but there are no
  materials for building steps available, finding some other way to get elevated
  will be a good idea that may help it get the object (including e.g. building a
  ladder, or asking someone tall to pick it up, etc.) 
- Figure out that, if the character wants to hide its valued object from a
  creature much larger than it, it should build a container with a small hole
  that the character can get through, but the creature cannot.

#### Physical Robot

Experiments were conducted using OpenCog and Nao Robot.

Interposing DeSTIN as a perception->action “black box” between OpenCog and a
robot can improve capability.

A a “white box” approach can lead to better results. This means  realtime use
of links between CogPrime nd DeSTIN internal networks.

### Natural Language Processing

[Nothing about atoms until this but cited]
[This part is not at its place]

The idea is to start with a *seed of rule-based NLP system* that will evolve
can evolve using OpenCog learning systems. The NLP system is both rule and
statistical based.

The NLP pipeline is described by the following diagram:

```
[Link Parser] -> [ReLex] -> [Link2Atom] -> {AtomSpace}
```

A combination of rule-based and statistical NLP tools have been integrated with
OpenCog, translating English into OpenCog atoms and vice versa:

- [Link Parser](http://www.abisource.com/projects/link-grammar/): maps English
  sentences into parse structures. Variants exists:

  - SAT link parser (based on Boolean satisfaction solver)
  - Viterbi link parser (try to closely simulate the human reading process)

- [RelEx](http://wiki.opencog.org/wikihome/index.php/RelEx_Dependency_Relationship_Extractor):
  translates Link Parser output into more abstract syntactico-semantic parses.
  Also handles some other odds and ends like anaphor resolution.

- [Link2Atom](http://wiki.opencog.org/wikihome/index.php/Link2Atom): translates
  RelEx output to OpenCog atoms

Also, NLGen2 has the task to build english sentend based on the results of the
above pipeline.

## Theory

OpenCog way of thinking about intelligent systems is guided by the patternist
[as in *pattern*] philosophy of the mind.

It's based on the premise that mind is made of pattern – and that a mind is a
system for recognizing patterns in itself and the world. An important kind of
pattern include patterns which **identify which procedures are likely to lead to
the achievement of which goals in which contexts**.

### What is a pattern?

In the patternist philosophy, "pattern" is generally defined as "representation
as something simpler." Thus, for example, if one measures simplicity in terms
of bit-count, then a program compressing an image would be a pattern of that
image. But if one uses a simplicity measure incorporating run-time as well as
bit-count, then the compressed version may or may not be a pattern of the image,
depending on how one’s simplicity measure weights of the two factors.

[replaced "in" by "of" in for exemple "of the image"]

*Pattern* definition includes simple repeated patterns and fractal patterns.

While pattern theory comes from computational theory, it is not tied to
computation; it can be developed in any context where there is a notion of
"representation" or "production" and a way of measuring simplicity

[skipped *pattern intensity* formula]

### What is intelligence?

In patternism the intelligent system is conceived as the (fuzzy) set of
patterns in that system, and the set of patterns emergent between that system
and other systems with which it interacts. The latter means that the patternist
perspective is inclusive of notions of distributed intelligence.

A mind is a collection of patterns that is associated with a persistent
dynamical process that achieves highly-patterned goals in highly-patterned
environments.

[XXX: What are the pattern I want to solve?]

Also in patternist philosophy of mind, reflection is critical to intelligence.

### Which patterns?

Patternism gives a structure to the tasks synthesizing intelligent systems.
We are led to ask questions such as:

- How are patterns represented in the system?
- How does the underlying infrastructure of the system give rise to the displaying of a particular pattern in the system’s behavior?
- What kinds of patterns are most compactly represented within the system?
- What kinds of patterns are most simply learned?
- What learning processes are utilized for recognizing patterns?
- What mechanisms are used to give the system the ability to introspect (so that it can recognize patterns in itself)? 

These same questions can be asked for “knowledge” or “information”. However, we
have found that asking these questions in the context of pattern leads to more
productive answers, avoiding unproductive byways and also tying in very nicely
with the details of various existing formalisms and algorithms for knowledge
representation and learning.

Among other kinds of patterns in intelligent systems, [semiotic patterns]()
are very interesting. [Peirce]() decomposed these into three categories:

- *iconic patterns*, which are patterns of contextually important internal
  similarity between two entities (e.g. an iconic pattern binds a picture of
  a person to that person)
- *indexical patterns*, which are patterns of spatiotemporal co-occurrence
  (e.g. an indexical pattern binds a wedding dress and a wedding)
- *symbolic patterns*, which are patterns indicating that two entities are often
  involved in the same relationships (e.g. a symbolic pattern between the number
  “5” (the symbol) and various sets of 5 objects (the entities that the symbol
  is taken to represent))

*symbolic patterns* have played an especially large role in the history of AI.

Mathematical logic and related formalisms provide sophisticated mechanisms for
combining and relating symbolic patterns. Some AI approaches have focused heavily
on these, more so than on **the identification of symbolic patterns in experience
or the use of them to achieve practical goals**.

#### How are organized those patterns?

Following from the view of intelligence in terms of achieving complex goals in
complex environments, comes a view in which the dynamics of a cognitive system
are understood to be governed by two main forces:

- *self-organization*, via which system dynamics cause existing system patterns
  to give rise to new ones
- *goal-oriented behavior*, basically a system interacting with its environment
  in a way to maximize some function.

Both are cooperative aspects.

[... posited ...]

[I don't understand the link between the previous paragraph a the paragraph I
skipped and the following paragraph]

[This introduce the “playing with blocks” scenario that is developed through
the book where a virtual agent in minecraft-like word is given the task
to impress some other agent with its creativity]

The principles [This explains more or less the same thing, lake of structure]:

#. *Evolution* [pattern query] process via which patterns within a large
   population thereof are selected and used as the basis for formation of new
   patterns, based on some “fitness function” that is generally tied to
   the goals of the agent.
#. *Autopoiesis* [pattern synergy] process via which a system of interrelated
   patterns maintains its integrity, whenever one of the patterns in the system
   begins to decrease in intensity, some of the other patterns increase their
   intensity which causes the troubled pattern to increase in intensity again.
#. *Association* [pattern cooperation] when patterns given attention they spread
   some of this attention to patterns they have previously been associated with.
   Furthermore there is Peirce’s *law of mind*, stating that the mind is an
   *associative memory network*, whose dynamics dictate that ideas of active
   agent, continually act on those ideas with which the memory associates
   [looks like this is related to Hebbian thing].
#. *Differential attention allocation / credit assignment* [pattern cooperation]
   Patterns that have been valuable for goal-achievement are given more
   attention, and are encouraged to participate in giving rise to new patterns.
#. *Pattern creation* Patterns that have been valuable for goal-achievement are
   mutated and combined with each other to yield new patterns. [same as above
   but the goal is stressed].

The network of patterns give rise to the following structures:

- *Hierarchical network* patterns are relations of control over other patterns
  that represent more specialized aspects of themselves.

- *Heterarchical network*. The system retains a memory of which patterns have
  previously been associated with each other in any way.

- *Dual network* Hierarchical and heterarchical structures are combined, with
  the dynamics of the two structures working together harmoniously. Among many
  possible ways to hierarchically organize a set of patterns, the one used
  should be one that causes hierarchically nearby patterns to have many
  meaningful heterarchical connections; and of course, there should be a
  tendency to search for heterarchical connections among hierarchically nearby
  patterns.
  
- *Self structure*. A portion of the network of patterns forms into an
  approximate image of the overall network of patterns.

The success of CogPrime will depend on whether these high-level structures and
dynamics can be made to emerge from the synergetic interaction of CogPrime's
representation and algorithms.

[Then a reference that seem important: [Building Better Minds: An Architecture for Artificial General Intelligence. In preparation, 2013.](http://goertzel.org/monkeyburger/bbm_main.pdf)]

