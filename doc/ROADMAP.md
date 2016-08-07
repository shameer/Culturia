## Roadmap

(Need an introduction)

### Done

Or mostly done.

- basic hypergraph (moving target)
- implement zorder packing aka. morton code
- cli, a framework for building command line interfaces (incomplete)


### Goals

#### datastore

Implement a datastore that is easy to scale and add custom datatypes. This
materialize in `culture.scm`.

Tasks:

- move to goops
- recursive 
- vcs
- indices
  - z-index (ongoing)
  - trigram index
  - full text index
- improve srfi 41 to match `gremlin` API
- graphical interface


#### NLP
  
- [text segmentation](https://en.wikipedia.org/wiki/Text_segmentation)
- lemmatizer
- syntax parser via Grammar Link
- dependency tree
- Implement a similar feature as wordvec: `king - man = queen`
- Summarization
  - LexRank
  - TextRank
  - TopicRank
- natural language interface
- Implement chat-80


#### Datasets

- datasets, create wrappers around <atom>

- conceptnet

- wikidata

- http://www.geonames.org/

...

#### Graph Algorithms 

- PageRank, SimRank, Personnalized PageRank

- other classic algorithms
