## Blog

### 0.0 - Youthful Indiscretion

This project started as an exploration of opencog hypergraph database.

Other documents written in the course of this journey:

- [dyna: extending datalog for modern ai](youthful-indiscretion/dyna-extending-datalog-for-modern-ai.htlm)

- [opencog overview](youthful-indiscretion/opencog-overview.html)

- [knowledge based ai (udacity)](youthful-indiscretion/udacity-knowledge-based-ai.html)

#### 2015/09/22 - What Are The Civilian Applications?

I started the the implementation and pushed some code. This includes
FFI bindings of wiredtiger with documentation and a few examples (that
may not work as-is). The best way to get started with wiredtiger is to
read
[Schema, Columns, Column Groups, Indices and Projections](http://source.wiredtiger.com/2.6.1/schema.html).

#### 2015/09/27 - Well I Was In The Neighbourhood

I tried several designs that includes:

**version control system**: this works using copy-on-write and
filtering stored atoms by *revision path*. Things that must be taken
into account:

- moving from one "workspace" to another. This requires to remove all
  changes that have been done in the "workspace". Other bookeeping
  scheme like transaction/multiple state revision so that it's
  possible for several thread to work on different *workspace*.

- and more interestingly provide an API that allows to browser the
  history of a single node and continue graph exploration from there.
  
**hierarchical hypergraph**: Simple hypergraph inheritance can use a
similar model to version control system. You build a tree (or
forest) using a set of tuples then that you cache in memory. Then
when fetching atoms, you check that those atoms are part of the
correct *tree branch path*
  
- *multiple parents hypergraph*: extending the above allowing parent atoms to
have multiple parents. Not sure what the use of this

I end up thinking really about recursive hypergraph where atoms could
freely link to other atoms but be part of a more general
structure. Then comes the idea of *recursive fuzzy hypergraph* which
really is the thing. It allows to model how for instance a “word” has
90% of time the sens of “an atomic semantic token of sentence” and 10%
of time the sens of “a message”. Given that it can be implemented
using a simple hypergraph efficently there is not need to go further.

Anyway, all this need more time to mature. Another thing I need to
tackle is the ability to run similar algorithm fully in memory and
database backed.

#### 2015/09/30 - Just Another Victim Of The Ambient Morality

I continued my quest for algorithms that could provide the required
operations to traverse the graph in original ways. Gremlin people seem
to stay deaf to my questions. Inference engines are original graph
algorithm compared to breath/depth first search and I can't really put
a name on it. Page Rank like algorithm (LexRank, TextRank) are nice
but they lake of realtime pragmatism.  How does an one restart Page
Rank over a graph that is already ranked. That said, there seem to be
research on the topic maybe Personnalized Page Rank.  Also the CPU
intensive, learning nature of Page Rank can be minimized by running PR
over a sub-graph or using CoSimRank. It seems also that the topology
open the door to simple ways to compute neighborhood even if it
requires prior learning
cf. [Is it possible to achieve something similar to word2vec](http://stackoverflow.com/questions/32851830/is-it-possible-to-achieve-something-similar-to-word2vec-using-a-graphdb).
I must remember to read about word2vec.

[Marko Rodriguez article on datastax dev blog, Tales from the TinkerPop, is full of graph-theory revelant links](http://www.datastax.com/dev/blog/tales-from-the-tinkerpop).

I started working on the traversi framework.

I've also been thinking about hyper[graph]{viz}(or). #mezangelle

#### 2015/10/05 - A Series Of Unlikely Explanations

In the previous paragraph, I did not talk about my research on the tasks of NLP
outside the word2vec things. There is different steps and level of
"understanding" of a sentence in NLP: lexical, syntaxic, semantic. The research
done as part of [Combinatorial Categorical Grammar](https://en.wikipedia.org/wiki/Combinatory_categorial_grammar)
are interesting. Even if some how it's different from dependency grammar.
It seems to me that having logical reprensentation of a sentence is the goal
of both methods. As the [tutorial explains](http://yoavartzi.com/tutorial/)
in CCG the method to infer this logic relations is via unsupervised learning.
This is really painful because a) I don't know that technics b) I don't like it
c) most importantly it requires an annotated corpus.

Nilas raised an important drawback of the approach I've taken with the database
where there no transient atom states and everything is propagated synchronously
to the hard disk. This can slow down significantly computation. I have real use
case example yet.

I had a quick look at [minikanren](http://minikanren.org) it looks like the
best candidate to implement my algorithms as soon as know which algorithm I
want to implement (!) which is still an open question. minikanren has
probabilistic version and a lot of documentation. The implementation is
straithforward. But I'm not there yet.

#### 2015/10/11 - Limiting Factor

I forgot to say previously that gremlin language is `srfi-41`
aka. scheme streams of graph vertices and edges. This is quiet a good
news I have less code to write.

While working on zindex support I got the brilliant idea to refactor
some `wiredtiger` code, that I've put in a `wiredtigerz` module. The
idea behind this is to *make it easier to define table, indices and
cursors* and add a few shortcuts to make the code more readable.

I discovered #ai@freenode.net and met a few interesting people.

Also, I'm still wondering what will be the main goal. I thinking about
Q/A and summarization. Q/A seems more involving. I need to have a look
at NLP coursera again. Or maybe, I should just code whatever comes to
my mind and forget about setting any goal...

#### 2015/10/14 - Profit Margin

I figured that what I am looking for is personnal assistant so it makes sens to
implement text mining tools like summarization and information retrieval (IR).

### 0.1 - Prime Mover

The road to 0.1 release.

#### 2016/08/08 - Determinist

I restart this project with the search engine previously named hyper
as a new basis.  I figured that if there is one application of NLP and
NLU; it is search engines.

Going through the work of building hyper, I discovered that I couldn't
crawl even small website as fast as I want. Also, most of my search
evolves around a few website which at least provides an API (or dumps
(or both)) to speed up the process of crawling. I will start with
offlining stackoverflow and then go on to do the same job for
hackernews and wikipedia at least.

The next step will be to port whoosh/lucene to guile.

Then I will restart thinking.
