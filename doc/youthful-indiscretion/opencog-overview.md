**OpenCog Sneak Peek**

Main goal: simplify *local cognitive load*, the rule is to :

- avoid opening doors
- focus and what are the directions of the project.

Move to an more algorithm, software centric overview while keeping the
background theory.

Notes taking algorithm:

- global: keep the intent ie. overview of opencog with research grounds
  - rework the document hierarchy to make more sens (FIXME)
  - keep the research background but make it less proeminent
  - move all the uncertainity and research work to a specific section (FIXME)

- local: 

  - simplify wording aka. avoid words I don't know. I'm sorry Yeats. Globish FTW!
  - simplify phrasing aka. less prose more bullets
  - avoid adjectives
  - avoid “because X, Y we should Z”  and instead “we must Z”

- glocal (key concept that forward to more general facts)
  - do not cite thing with no prior proper introduction (except glocal)
  - less implementation purity facts like "MOSES handles *much of* procedural
    learning" prefer "MOSES handle procedural learning"
  - less contextualisation like "computationally feasible", "mainstream in IA" etc..
  - focus contextually relevant facts instead of trying to be comprehensive
  - strip examples [FIXME: add examples]

**The goal is to move to this table of content**

- Introduction
  - AI vs AGI
  - {Open,Cog,Prime}
  - Secret Sauce
  - Key Claims

- Theory (Pattern Theory of the Mind)
- Implementation
  - AtomSpace
  - Natural Language Pipeline
  - MOSES
  - DeSTIN
  - PLN
  - ...
- Applications
- Roadmap

# Introduction

## AI vs AGI

AI is really narrow-AI. AGI also know as *Human Level Intelligence* is a revival
of original AI goals to create a human-like intelligence.

## {Open, Cog, Prime}

- OpenCog is a framework
- CogPrime is an AGI
- OpenCogPrime is the implementation of CogPrime using OpenCog (proprietary)

## What Kind of Intelligence is OpenCog Aimed At?

Pratical goals:

- *Turing Test*: the classic, involving passing as a human being in an everyday
   conversation
- *Virtual World* or *Telerobotic Turing Test* : pass as a human being, in a
   conversation involving controlling a virtual world avatar or a robot,
   alongside verbal interaction 
- *Online University Test*: attend an online university just like a human
   student, and graduate
- *Physical University Test*: control a robot that attends university just
   like a human student, and graduates
- *Artificial Scientist Test*: write and publish original science papers,
   based on ideas conceived by the AI due to its own reading of the literature 

[There is also an argument about “ressources” and “limited ressources”.
I'll paraphrase  Alan Kay argument “To build the software of the future
use the ressources of the future”
[https://www.youtube.com/watch?v=gTAghAJcO1o](The Future doesn't have to be incremental)]

## What's the secret sauce

*Secret sauce*: Making diverse AI algorithms with diverse background theories
work together in a *synergetic* and *cooperative* way.

*white boxes* instead of *black boxes*.

Another important aspect of intelligence is *adaptiveness*: the capability of a
system to adapt to its environment [recognize similar problems and apply
previous structure and algorithm] effectively.

A key principle is: the use of multiple cognitive processes
associated with multiple types of memory to enable an intelligent agent
to execute the procedures that it believes have the best probability of
working toward its goals in its current context.

There is no particular new algorithm or architecture principle.

- *synergy*: the interaction of elements that when combined produce a total
  effect that is greater than the sum of the individual elements, contributions,
  etc.

## Key Claims

This is a list of claims that the reader must accept to validate that OpenCog
approach to AGI is a viable one.

An AGI system must:

- Use much of its *resources*
- Represent different kind of knowledge via different kinds of *cognitive memory*:
  - declarative
  - procedural
  - episodic
  - sensory
  - intentional
  - attentional

- Use *cognitive synergy* between different cognitive process to overcome
  processing bottlenecks. This means that cognitive processus must be able to
  understand and use the memory of each other.
- have a global, local and *glocal* memory
- have sensory data and motoric affordances that roughly emulate those
  available to humans.
- must grow/evolve up along a path roughly comparable to that followed by human
  children. It is not human intelligence man, it learns human intelligence.
- learning must happend using a mix of spontaneous learning and explicit
  instricution:
   - imitation
   - reinforcement and correction
   - linguistic and nonlinguistic instruction
- learn human language via built-in NLP facility that can be improved based on
  experiences. NLP facility must be both rule and statistic based.
- recognize and represent large-scale pattern in itself called the *cognitive equation*
  [reference: Chaotic Logic. Plenum, 1994. BibTeX [Goe94]].
- upon sustained interaction with an environment in pursuit of goals emerge
   internal knowledge network, including but not limited to:
   - *hierarchical network: representing both a spatiotemporal hierarchy and an
     approximate “default inheritance” hierarchy, cross-linked*
   - [heterarchical network](https://en.wikipedia.org/wiki/Heterarchy) *of
     associativity, roughly aligned with the hierarchical network*
   - *self network which is an approximate micro image of the whole network*
   - *inter-reflecting networks modeling self and others, reflecting a
     “mirrorhouse” design pattern*
     [reference: Mirror Neurons, Mirrorhouses, and the Algebraic Structure of the Self GASP08].
- implement a simplicity bias in each cognitive process cf. [Occam’s Razor](https://en.wikipedia.org/wiki/Occam's_razor).
- if supplied with a commonsensically ethical goal system and an intentional
   component based on rigorous uncertain inference, should be able to reliably
   achieve a much higher level of commonsensically ethical behavior than any
   human being.
- once sufficiently advanced, an AGI system with a logic-based declarative
   knowledge approach and a program-learning-based procedural knowledge approach
   should be able to radically self-improve via a variety of methods, including
   supercompilation and automated theorem-proving. 

Efficiency is not a side-issue but rather the essence of real-world AGI
[reference: Goertzel, Ben. Toward a Formal Definition of Real-World General
Intelligence.].
Hutter has shown, if one casts efficiency aside, arbitrary levels of general
intelligence can be achieved via a trivially simple program

[In the original article this was a point in the above list, this goes a bit
further in the definition of what an AGI must do and instead dive into how
it should do it, describing all the components of OpenCog]

Given the strengths and weaknesses of current and near-future digital computers:

- *a (loosely) neural-symbolic network is a good representation for
  directly storing many kinds of memory, and interfacing between those
  that it doesn’t store directly;*
- *Uncertain logic is a good way to handle declarative knowledge. To
  deal with the problems facing a human-level AGI, an uncertain logic
  must integrate imprecise probability and fuzziness with a broad
  scope of logical
  constructs. [PLN](http://wiki.opencog.org/wikihome/index.php/Probabilistic_Logic_Networks)
  is one good realization.*
- *Programs are a good way to represent procedures (both cognitive and
  physical-action, but perhaps not including low-level motor-control
  procedures).*
- *Evolutionary program learning is a good way to handle difficult
  program learning problems. Probabilistic learning on normalized
  programs is one effective approach to evolutionary program
  learning. [MOSES](http://wiki.opencog.org/wikihome/index.php/Meta-Optimizing_Semantic_Evolutionary_Search)
  is one good realization of this approach.*
- Multistart hill-climbing, with a strong Occam prior, is a good way
  to handle relatively straightforward program learning problems.
- Activation spreading and Hebbian learning comprise a reasonable way
  to handle attentional knowledge (though other approaches, with
  greater overhead cost, may provide better accuracy and may be
  appropriate in some situations).
  - Artificial economics is an effective approach to activation
    spreading and Hebbian learning in the context of neural-symbolic
    networks;
  - ECAN is one good realization of artificial economics;
  - A good trade-off between comprehensiveness and efficiency is to
    focus on two kinds of attention: processor attention (represented
    in CogPrime by ShortTermImportance) and memory attention
    (represented in CogPrime by LongTermImportance).
- Simulation is a good way to handle episodic knowledge (remembered
  and imagined). Running an internal world simulation engine is an
  effective way to handle simulation.
- Hybridization of one’s integrative neural-symbolic system with a
  spatiotemporally hierarchical deep learning system is an effective
  way to handle representation and learning of low-level sensorimotor
  knowledge. DeSTIN is one example of a deep learning system of this
  nature that can be effective in this context.
- One effective way to handle goals is to represent them
  declaratively, and allocate attention among them
  economically. CogPrime ’s PLN/ECAN based framework for handling
  intentional knowledge is one good realization.

[Missing reference to [Relex](http://wiki.opencog.org/wikihome/index.php/RelEx_Dependency_Relationship_Extractor) and GrammarLink]

# Theory

## Pattern based theory of the mind

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

[Implicit in the patternist theory patterns are graph patterns]

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

### How are organized those patterns?

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

- *Evolution* [pattern query] process via which patterns within a large
   population thereof are selected and used as the basis for formation of new
   patterns, based on some “fitness function” that is generally tied to
   the goals of the agent.
- *Autopoiesis* [pattern synergy] process via which a system of interrelated
   patterns maintains its integrity, whenever one of the patterns in the system
   begins to decrease in intensity, some of the other patterns increase their
   intensity which causes the troubled pattern to increase in intensity again.
- *Association* [pattern cooperation] when patterns given attention they spread
   some of this attention to patterns they have previously been associated with.
   Furthermore there is Peirce’s *law of mind*, stating that the mind is an
   *associative memory network*, whose dynamics dictate that ideas of active
   agent, continually act on those ideas with which the memory associates
   [looks like this is related to Hebbian thing].
- *Differential attention allocation / credit assignment* [pattern cooperation]
   Patterns that have been valuable for goal-achievement are given more
   attention, and are encouraged to participate in giving rise to new patterns.
- *Pattern creation* Patterns that have been valuable for goal-achievement are
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

[Building Better Minds: An Architecture for Artificial General Intelligence. In preparation, 2013.](http://goertzel.org/monkeyburger/bbm_main.pdf) elaborate how these concepts arises concretely from CogPrime's structures and algorithms.

## Mind-World Correspondence Principle

An additional philosophical principle has guided CogPrime design; this is
the "mind-world correspondence principle", which enlarges on the notion of
"intelligence as adaptation to environments".

For intelligence to occur, there has to be a natural correspondence between
the transition-sequences of world-states and the corresponding
transition-sequences of mind-states, at least in the cases of
transition-sequences leading to relevant goals. 

## High-Level Architecture of CogPrime

[Figure depecting the different cognitive process with interactions](http://wiki.opencog.org/wikihome/index.php/File:1-no_gray.jpg).

## Local, Global and Glocal

The two major supercategories of knowledge representation systems are *local* also called *explicit* and *global* also called *implicit* systems, with a hybrid category *glocal* that combines both local and global (explicit and implicit).

In CogPrime all three are realized using the same network AtomSpace.

In the following we discuss:

- the symbolic, semantic-network aspects of knowledge representation
- distributed, neural-net-like knowledge representation, focusing on CogPrime’s
  *glocal* knowledge representation

# OpenCog framework

As described above OpenCog is an opionated framework which takes its root
in the following principles:

- Patternist theory of the mind
- Expert cognitive processus
- Synergy between processus

## Natural Language Processing Pipeline

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

## AtomSpace

Weigthed, Labeled Hypergraph.

There are different knowledge representations in AI systems in an explicit, localized way, most of them descending from formal logic.

CogPrime's explicit localized knowledge representation use a generalized
hypergraph, a [graph]() consisting of:

- Node that can embed hypergraphs
- Link that can connect more that two nodes and can connect to other links

In OpenCog, Node and Link inherit the Atom type.

*generalized hypergraph* are always refered as simply *hypergraph*.

[XXX: need an example of the above in opencog]

[XXX: is it possible to link a hypernode in an embedded hypergraph to and
hypernode outside the embedded hypergraph]

[XXX: In culturia, atoms are really hyper-links and there is no hyper-nodes.
It might be possible to implement embedded hypergraph on top. hierarchical
hypergraph doesn't count as embedded hypergraph]

Atoms comes with type, name and weights.

An hypergraph comes with the following dynamics:

- modify the properties of atoms such as the labels or weights
- add new atoms
- remove existing ones 

### Atoms Types and Weights

It is important to note that the AtomSpace is neither a *neural net* nor a
*semantic net*.

It is not a *neural net* because it has no activation values, and involves no
attempts at low-level brain modeling. However, attention values are very loosely
analogous to time-averages of neural net activations.

It is not a *semantic net* because of the broad scope of the Atoms in the
network: for example, Atoms may represent percepts, procedures, or parts of
concepts.

Most CogPrime Atoms have no corresponding English label. However, most CogPrime
Atoms do have probabilistic truth values, allowing logical semantics. 

[XXX: This changed to Generic Truth Values]

Atoms can be quantified with *truth values* that can have two components, one
representing *probability* (strength) and the other representing
*weight of evidence*. There is also *attention values* that have two components,
*short-term* and *long-term importance*.

#### Atoms 

##### GetLink, PutLink and BindLink interaction

https://groups.google.com/forum/?hl=fr#!topic/opencog/jXzPY7sJeUM


### Glocal Memory

[reference](http://wiki.opencog.org/wikihome/index.php/CogPrime_Overview#Glocal_Memory)

*Glocal coordination of local and global memory*.

*Glocal memory* applies to many forms of memory; We will focus on perceptual and
declarative memory.

[XXX: I assume AtomSpace stores perceptual memory too, but what else?]

The central idea of *glocal memory* is that perceptual, declarative, episodic,
procedural, etc. items can be stored in memory in the form of paired structures
that are called `(key, map)` pairs.

The `key` is a localized version of the item, and records some significant
aspects of the items in a simple and crisp way. [It's a pattern for the items]

The `map` is a dispersed, distributed version of the item, which represents the
item as a (to some extent, dynamically shifting) combination of fragments of
other items.

*glocality* lies at the heart of this combination:

- *Local knowledge* ie. the `key` is represented in abstract logical
  relationships stored in explicit logical form, and also in [Hebbian]()-type
  associations between atoms
  
- *Global knowledge* is represented in large-scale patterns of atoms
  weights, which lead to large-scale patterns of network activity, which often
  take the form of attractors qualitatively similar to [Hopfield net attractors]().
  These attractors are called maps. 

Activation of either one of the two tends to also activate the other one.

[XXX: *glocality* involves building/compiling local representations of global
knowledge that is relevant to the local memory serving as a `key` for
the global knowledge in the considered context. The `key` is a locally relevant,
activation *pattern* for a global knowledge. The difference between the
glocal construction and a regular `map` or `hashmap` is that the the glocal
`key` is part of the bigger memory it's refering to so it can be activated as
part of this memory too: That's why the `key` can be activated by the global
memory. For instance a «cat» memory can contain two keys
«lol cat from the interwebs» and «wild cats» both remain in the same global
memory of «cats» but have their identity specialized and linked to other
memory.]

### Memory Types and Associated Cognitive Processes in CogPrime

We dig deeper into the internals of the CogPrime approach, turning to aspects
of the *relationship between structure and dynamics*.

CogPrime's memory types are:

- declarative,
- procedural,
- sensory,
- episodic
- attentional memory for allocating system resources generically,
- intentional memory for allocating system resources in a goal-directed way.

In terms of patternist cognitive theory, the multiple types of memory in
CogPrime should be considered as specialized ways of storing particular types
of pattern, optimized for spacetime efficiency.

[Like explained in the "What's the Secret Sauce"], the gist of CogPrime
architecture is *cooperation* and *cognitive synergy*.

*Cognitive synergy* is implemented by the ability given to process to convert
their memory in the terms of other process memory.

#### Cognitive Synergy in Probabilistic Logic Network

[reference](http://wiki.opencog.org/wikihome/index.php/CogPrime_Overview#Cognitive_Synergy_in_PLN)

Let's elaborate on the role it plays in the interaction between *procedural and
declarative learning*:

- MOSES handle procedural learning
- CogPrime simulation engine handle *episodic knowledge* [XXX: memory?]
- Probabilistic Logic Networks (PLN), an *uncertain inference framework*,
  handles declarative knowledge

PLN seeks to achieve efficient inference control via integration with other
cognitive processes.

As a logic, PLN is broadly integrative: it combines certain term logic rules
with more standard predicate logic rules, and utilizes both fuzzy truth values
and a variant of imprecise probabilities called indefinite probabilities. PLN
mathematics tells how these uncertain truth values propagate through its logic
rules, so that uncertain premises give rise to conclusions with reasonably
accurately estimated uncertainty values.

PLN can be used in either forward or backward chaining mode; and in the language
introduced above, it can be used for either analysis or synthesis.

The combinatorial explosion of inference control is combatted by defering to other
cognitive processes when the inference control procedure is unable to make
a sufficiently confident choice of which inference steps to take next.

MOSES may rely on PLN to model its evolving populations of procedures, PLN may
rely on MOSES to create complex knowledge about the terms in its logical
implications. This is just one example of the multiple ways in which the
different cognitive processes in CogPrime interact synergetically.

### Goal-Oriented Dynamics

[reference](http://wiki.opencog.org/wikihome/index.php/CogPrime_Overview#Goal-Oriented_Dynamics_in_CogPrime)

CogPrime’s dynamics has both goal-oriented and “spontaneous” aspects. [XXX:
should be introduction]

### Clarifying the Key Claims

[reference](http://wiki.opencog.org/wikihome/index.php/CogPrime_Overview#Clarifying_the_Key_Claims)

#### Mutli-Memory Systems

It’s important that an AGI system can handle different kinds of memory (declarative, procedural, episodic, sensory, intentional, attentional) in customized but interoperable ways.

[XXX: because of space-time efficiency]

In cases where the same representational mechanism is used for different types
of knowledge, different cognitive processes are used, and often different
aspects of the representation (e.g. attentional knowledge is dealt with largely
by ECAN acting on AttentionValues and HebbianLinks in the AtomSpace; whereas
declarative knowledge is dealt with largely by PLN acting on TruthValues and
logical links, also in the AtomSpace).

In fact the multi-memory approach may have a broader importance, even to
intelligences without multimodal communication.

Decades of computer science and narrow-AI practice strongly suggest that
the“ one memory structure fits all” approach is not capable of leading
to effective real-world approaches.

#### Preception, Action and Environment

#### Developmental Pathways

One important case of learning that human children are particularly
good at is language learning; It is very tempting to give AGI systems
a “short cut” to language proficiency via making use of existing
rule-based and statistical-corpus-analysis-based NLP systems;

#### Knowledge Representation

The key goal for a knowledge representation for AGI should be
naturalness with respect to the AGI’s cognitive processes – i.e. the
cognitive processes shouldn’t need to undergo complex transformative
gymnastics to get information in and out of the knowledge
representation in order to do their cognitive work.

A neural-symbolic network is a good representation for directly
storing many kinds of memory, and interfacing between those that it
doesn’t store directly AtomSpace is a neural-symbolic network designed
to work nicely with PLN, MOSES, ECAN and the other key CogPrime
cognitive processes; it supplies them with what they need without
causing them undue complexities.

#### Cognitive Processes

The crux of intelligence is dynamics, learning, adaptation; 

Given CogPrime’s multi-memory design, it’s natural to consider CogPrime’s
cognitive processes in terms of which memory subsystems they focus on even if
some cognitive process spans multiple memory.

##### Uncertain Logic for Declarative Knowledge

The PLN logic framework is one way of integrating imprecise
probability and fuzziness in a logical formalism that encompasses a
broad scope of logical constructs.

It integrates term logic and predicate logic

##### Program Learning for Procedural Knowledge

In designing CogPrime , we have acted based on the understanding that
programs are a good way to represent procedures – including both
cognitive and physical-action procedures, but perhaps not including
low-level motor-control procedures.

Using a special language called Combo that is essentially a minor
variant of LISP. What differentiates this use of LISP from many
traditional uses of LISP in AI is that we are only using the LISP-ish
representational style for procedural knowledge, rather than trying to
use it for everything.

Low level procedures are represented using DeSTIN.

With both it's possible to make a Combo program that  invoke DeSTIN procedures,
and encode higher-level actions like “pick up the cup in front of you slowly and
quietly, then hand it to Jim who is standing next to you.” 

Having committed to use programs to represent many procedures, the next
question is how to learn programs:

- for straightforward learning problems, *hillclimbing with random restart* and a
  *strong Occam bias* is an effective method
- for other problems, *probabilistic evolutionary program learning* (MOSES) is
  an effective method

##### Attention Allocation

- Activation spreading is a reasonable way to handle attentional knowledge.
- Hebbian learning as one route of learning associative relationships (with more
  sophisticated methods such as information-geometric ones potentially also
  playing a role)

Where CogPrime differs from standard practice is in the use of an economic
metaphor to regulate activation spreading.

In the context of a neural-symbolic network, artificial economics is an
effective approach to activation spreading; and CogPrime’s ECAN framework seeks
to embody this idea.

One major choice made in the CogPrime design is to focus on two kinds
of attention: processor (represented by ShortTermImportance) and
memory (represented by LongTermImportance)

##### Internal Simulation and Episodic Knowledge

Simulation is a good way to handle episodic knowledge; and running an
internal “world simulation engine” is an effective way to handle
simulation.

##### Low-Level Perception and Action

Hybridization of one’s integrative neural-symbolic system with a
spatiotemporally hierarchical deep learning system is an effective way
to handle representation and learning of low-level sensorimotor
knowledge. DeSTIN is one example of a deep learning system of this
nature that can be effective in this context.

##### Goals

Given that we have characterized general intelligence as “the ability
to achieve complex goals in complex environments,” However, we have
chosen not to create a separate subsystem for intentional knowledge,
and instead have concluded that one effective way to handle goals is
to represent them declaratively, and allocate attention among them
economically.

Goals and subgoals are related using logical links as interpreted and
manipulated by PLN, and attention is allocated among goals using the
STI dynamics of ECAN, and a specialized variant.

Thus the mechanics of goal management is handled using uncertain
inference and artificial economics, whereas the figuring-out of how to
achieve goals is done integratively, relying heavily on procedural and
episodic knowledge as well as PLN and ECAN.

The combination of ECAN and PLN seems to overcome the well-known
shortcomings found with purely neural-net or purely inferential
approaches to goals. Neural net approaches generally have trouble with
abstraction, whereas logical approaches are generally poor at
real-time responsiveness and at tuning their details quantitatively
based on experience.

##### Fulfilling the Cognitive Equation

It is important for an intelligent system to have some way of
recognizing large-scale patterns in itself, and then embodying these
patterns as new, localized knowledge items in its memory.

This dynamic introduces a feedback dynamic between emergent pattern and
substrate, which is critical to general intelligence. It also ties in nicely
with the notion of “glocal memory” – essentially positing a localization of
some global memories, which naturally will result in the formation of some
glocal memories. One of the key ideas underlying the CogPrime design is that
given the use of a neural-symbolic network for knowledge representation, a
graph-mining based “map formation” heuristic is one good way to do this. 

Map formation seeks to fulfill the Cognitive Equation directly. Rather than
relying on other cognitive processes to implicitly recognize overall system
patterns and embody them in the system as localized memories (though this
implicit recognition may also happen), the MapFormation MindAgent explicitly
carries out this process. Mostly this is done using fairly crude greedy pattern
mining heuristics, though if really subtle and important patterns seem to be
there, more sophisticated methods like evolutionary pattern mining may also be
invoked.

##### Occam's Razor

This quest for simplicity is present in many places throughout the
CogPrime design, for instance

- In MOSES and hillclimbing, where program compactness is an explicit
  component of program tree fitness
- In PLN, where the backward and forward chainers explicitly favor
  shorter proof chains, and intensional inference explicitly
  characterizes entities in terms of their patterns (where patterns
  are defined as compact characterizations)
- In pattern mining heuristics, which search for compact characterizations of data
- In the forgetting mechanism, which seeks the smallest set of Atoms
  that will allow the regeneration of a larger set of useful Atoms via
  modestly-expensive application of cognitive processes
- Via the encapsulation of procedural and declarative knowledge in
  simulations, which in many cases provide a vastly compacted form of
  storing real-world experiences

Like cognitive synergy and emergent networks, Occam’s Razor is not
something that is implemented in a single place in the CogPrime
design, but rather an overall design principle that underlies nearly
every part of the system.

##### Cognitive Synergy

These synergies are absolutely critical to the proposed functionality
of the CogPrime system.  Without them, the cognitive mechanisms are
not going to work adequately well, but are rather going to succumb to
combinatorial explosions.

The other aspects of CogPrime - the cognitive architecture, the knowledge
representation, the embodiment framework and associated developmental teaching
methodology - are all critical as well, but none of these will yield the
critical emergence of intelligence without cognitive mechanisms that effectively
scale.

And, in the absence of cognitive mechanisms that effectively scale on their own,
we must rely on cognitive mechanisms that effectively help each other to scale.

###### Synergies that Help Inference

The combinatorial explosion in PLN is obvious: forward and backward chaining
inference are both fundamentally explosive processes, reined in only by pruning
heuristics.

For nontrivial complex inferences to occur, one needs really, really clever
pruning heuristics.

The CogPrime design combines simple heuristics with pattern mining, MOSES and
economic attention allocation as pruning heuristics. Economic attention
allocation assigns importance levels to Atoms, which helps guide pruning. 
Greedy pattern mining is used to search for patterns in the stored corpus of
inference trees, to see if there are any that can be used as analogies for the
current inference.

And MOSES comes in when there is not enough information (from importance levels
or prior inference history) to make a choice, yet exploring a wide variety of
available options is unrealistic.

###### Synergies that Help MOSES

MOSES’s combinatorial explosion is obvious: the number of possible
programs of size N increases very rapidly with N. The only way to get
around this is to utilize prior knowledge, and as much as possible of
it. When solving a particular problem, the search for new solutions
must make use of prior candidate solutions evaluated for that problem,
and also prior candidate solutions (including successful and
unsuccessful ones) evaluated for other related problems.

But, extrapolation of this kind is in essence a contextual analogical
inference problem. In some cases it can be solved via fairly
straightforward pattern mining; but in subtler cases it will require
inference of the type provided by PLN. Also, attention allocation
plays a role in figuring out, for a given problem A, which problems B
are likely to have the property that candidate solutions for B are
useful information when looking for better solutions for A.

##### Synergies that Help Attention Allocation

Economic attention allocation, without help from other cognitive
processes, is just a very simple process analogous to “activation
spreading” and “Hebbian learning” in a neural network. The other
cognitive processes are the things that allow it to more sensitively
understand the attentional relationships between different knowledge
items (e.g. which sorts of items are often usefully thought about in
the same context, and in which order).

#####  Further Synergies Related to Pattern Mining

[Further Synergies Related to Pattern Mining]

Statistical, greedy pattern mining is a simple process, but it
nevertheless can be biased in various ways by other, more subtle
processes.

For instance, if one has learned a population of programs via MOSES,
addressing some particular fitness function, then one can study which
items tend to be utilized in the same programs in this population. One
may then direct pattern mining to find patterns combining these items
found to be in the MOSES population. And conversely, relationships
denoted by pattern mining may be used to probabilistically bias the
models used within MOSES.

Statistical pattern mining may also help PLN by supplying it with
information to work on. For instance, conjunctive pattern mining finds
conjunctions of items, which may then be combined with each other
using PLN, leading to the formation of more complex predicates. These
conjunctions may also be fed to MOSES as part of an initial population
for solving a relevant problem.

Finally, the main interaction between pattern mining and MOSES/PLN is
that the former may recognize patterns in links created by the
latter. These patterns may then be fed back into MOSES and PLN as
data. This virtuous cycle allows pattern mining and the other, more
expensive cognitive processes to guide each other. Attention
allocation also gets into the game, by guiding statistical pattern
mining and telling it which terms (and which combinations) to spend
more time on.

##### Synergies Related to Map Formation

[Synergies Related to Map Formation]

The essential synergy regarding map formation is obvious: Maps are
formed based on the HebbianLinks created via PLN and simpler
attentional dynamics, which are based on which Atoms are usefully used
together, which is based on the dynamics of the cognitive processes
doing the “using.” On the other hand, once maps are formed and
encapsulated, they feed into these other cognitive processes. This
synergy in particular is critical to the emergence of self and
attention.

What has to happen, for map formation to work well, is that the
cognitive processes must utilize encapsulated maps in a way that gives
rise overall to relatively clear clusters in the network of
HebbianLinks. This will happen if the encapsulated maps are not too
complex for the system’s other learning operations to understand. So,
there must be useful coordinated attentional patterns whose
corresponding encapsulated-map Atoms are not too complicated. This has
to do with the system’s overall parameter settings, but largely with
the settings of the attention allocation component. For instance, this
is closely tied in with the limited size of “attentional focus” (the
famous 7 +/- 2 number associated with humans’ and other mammals short
term memory capacity). If only a small number of Atoms are typically
very important at a given point in time, then the maps formed by
grouping together all simultaneously highly important things will be
relatively small predicates, which will be easily reasoned about -
thus keeping the “virtuous cycle” of map formation and comprehension
going effectively.

#### Emergent Structures and Dynamics

We have spent much more time in this book on the engineering of
cognitive processes and structures, than on the cognitive processes
and structures that must emerge in an intelligent system for it to
display human-level AGI. However, this focus should not be taken to
represent a lack of appreciation for the importance of
emergence. Rather, it represents a practical focus: engineering is
what we must do to create a software system potentially capable of
AGI, and emergence is then what happens inside the engineered AGI to
allow it to achieve intelligence. Emergence must however be taken
carefully into account when deciding what to engineer!

One of the guiding ideas underlying the CogPrime design is that an AGI
system with adequate mechanisms for handling the key types of
knowledge mentioned above, and the capability to explicitly recognize
large-scale pattern in itself, should upon sustained interaction with
an appropriate environment in pursuit of appropriate goals, emerge a
variety of complex structures in its internal knowledge network,
including (but not limited to): a hierarchical network, representing
both a spatiotemporal hierarchy and an approximate “default
inheritance” hierarchy, cross-linked; a heterarchical network of
associativity, roughly aligned with the hierarchical network; a self
network which is an approximate micro image of the whole network; and
inter-reflecting networks modeling self and others, reflecting a
“mirrorhouse” design pattern.

The dependence of these posited emergences on the environment and
goals of the AGI system should not be underestimated. For instance,
PLN and pattern mining don’t have to lead to a hierarchical structured
AtomSpace. But if the AGI system is placed in an environment which it
hierarchically structured via its own efforts, thenPLN and pattern
mining very likely will lead to a hierarchically structured
AtomSpace. And if this environment consists of hierarchically
structured language and culture, then what one has is a system of
minds with hierarchical networks, each reinforcing the hierarchality
of each others’ networks. Similarly, integrated cognition doesn’t have
to lead to mirrorhouse structures, but integrated cognition about
situations involving other minds studying and predicting and judging
each other, is very likely to do so. What is needed for appropriate
emergent structures to arise in a mind, is mainly that the knowledge
representation is sufficiently flexible to allow these structures, and
the cognitive processes are sufficiently intelligent to observe these
structures in the environment and then mirror them internally. Of
course, it also doesn’t hurt if the internal structures and processes
are at least slightly biased toward the origination of the particular
high-level emergent structures that are characteristic of the system’s
environment/goals; and this is indeed the case with CogPrime
... biases toward hierarchical, heterarchical, dual and mirrorhouse
networks are woven throughout the system design, in a thoroughgoing
though not extremely systematic way.

# Applications

- PLN + Relex: automated biological hypothesis generation based on information
  gathered from PubMed abstracts
- MOSES component for biological data analysis;
- financial prediction, genetics, marketing data analysis and natural language
  processing.

Most relevant: OpenCog has also been used to control agents in virtual worlds

## Virtual Agents

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

## Physical Robot

Experiments were conducted using OpenCog and Nao Robot.

Interposing DeSTIN as a perception->action “black box” between OpenCog and a
robot can improve capability.

A a “white box” approach can lead to better results. This means  realtime use
of links between CogPrime nd DeSTIN internal networks.

## Build Me Something I Haven't Seen Before

...

# Roadmap

## Measuring Progress toward AGI

[reference](http://wiki.opencog.org/wikihome/index.php/CogPrime_Overview#Measuring_Incremental_Progress_Toward_Human-Level_AGI)

## Technical Roadmap

http://wiki.opencog.org/wikihome/index.php/OpenCogPrime:Roadmap
http://wiki.opencog.org/wikihome/index.php/Roadmap

# Glossary

- AtomSpace: is a neural-symbolic network designed to work nicely with PLN, MOSES, ECAN and the other key CogPrime cognitive processes;
- Atom
- CogPrime
- Cognitive synergy
- Cognitive process
- Declarative Learning
- DeSTIN: http://wiki.opencog.org/wikihome/index.php/DeSTIN
- LinkGrammar
- Memory
- Memory (declarative)
- Memory (procedural)
- Memory (sensory)
- Memory (episodic)
- Memory (attentional) for allocating system resources generically,
- Memory (intentional) for allocating system resources in a goal-directed way.
- MOSES: handle procedural learning
- OpenCog
- OpenCogPrime
- patternist cognitive theory
- Pattern
- Probabilitstic Logic Network (PLN): uncertain inference framework
- Procedural Learning
- ReLex

# Links

- [CogPrime Overview](http://wiki.opencog.org/wikihome/index.php/CogPrime_Overview)
- [Bibliography of OpenCog](http://wiki.opencog.org/wikihome/index.php/Background_Publications)

