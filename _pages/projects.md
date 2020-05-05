---
layout: page
title: Projects
permalink: /projects
weight: 30
---

This is a categorized list of much of the software I have worked on. Some things are listed twice if I wasn't sure what category to put them in. Source code for a lot of these projects can also be found on [my github profile](https://github.com/tyehle).

{% include toc.html tags="H2,H3" %}


---

Functional Programming
----------------------
- A compiler for a simple language with lambdas and a garbage collector using LLVM [[src](https://github.com/tyehle/llvm-lambda)]
- A [compiler from scheme to the λ-calculus]({% post_url 2017-01-23-lambda-compiler %})[[src](https://github.com/tyehle/lambda)]
- Most of a [Python compiler]({% post_url 2015-10-31-python_compiler %}). This project was for a compilers class, and we implemented a lexer, parser, desugarer, and normalizer.
- A [Dominion AI]({% post_url 2016-07-29-dominion %}) [[src](https://github.com/tyehle/dominion)]
- An interesting sudoku solver in Haskell [[src](https://github.com/tyehle/sudoku)]
- An implementation of Java style interfaces in an interpreted lisp-like language
- A regex matcher using derivatives in JavaScript


---

Graphics
--------
- A 3D engine with a nonlinear projection, written in Rust and OpenGL [[src](https://github.com/tyehle/fieldgame-rust)]
- A prototype version of the 3D engine written in pure scala with a demo game on top [[src](https://github.com/tyehle/fieldgame)]
- An ASCII art generator [[src](https://bitbucket.org/tobinyehle/ascii-converter)]


---

Hardware
--------


---

Data Engineering
----------------
- A data ETL pipeline for 3M HIS's data science lab
- Deployment of ML models on AWS Lambda for 3M HIS's data science lab


---

Robotics
--------


---

Machine Learning and AI
-----------------------
- A question answering system based heavily on [quarc](https://www.cs.utah.edu/~riloff/pdfs/quarc.pdf)
- [Boggle solver]({% post_url 2016-10-4-boggle %}) [[src](https://bitbucket.org/tobinyehle/bogglesolver)]
- A [Dominion AI]({% post_url 2016-07-29-dominion %}) [[src](https://github.com/tyehle/dominion)]
- A low performance character recognition algorithm for sheet music
- Neural nets with backprop training
- ID3 decision trees
- SVM with various loss functions and SGD training
- A lame sudoku solver [[src](https://bitbucket.org/tobinyehle/sudoku-solver)]

### Thesis
I did my undergrad thesis in natural language parsing with [Dr. Vivek Srikumar](http://svivek.com) at the University of Utah.
- **Memoized Parsing with Derivatives**: Tobin Yehle (2016) [pdf](resources/thesis.pdf)


---

Simulation
----------
- A dynamical friction simulation [[src](https://github.com/tyehle/dynamical-friction)]
- Particle simulation [[src](https://github.com/tyehle/particles)]


---

Complex Networks
----------------
- Clustering analysis of suicide cases based on medical diagnostic codes for the U of U departement of Psychiatry

### Publications
I worked on complex networks with [Dr. Ronaldo Menezes](http://cs.fit.edu/~rmenezes/Home.html) at Florida Institue of Technology.
- **From Criminal Spheres of Familiarity to Crime Networks**:
  Marcos A. C. Oliveira, Hugo Serrano Barbosa Filho, Tobin Yehle, Sarah White, Ronaldo Menezes (2015)
  [doi](http://dx.doi.org/10.1007/978-3-319-16112-9_22)

- **The Spatial Structure of Crime in Urban Environments**:
  Sarah White, Tobin Yehle, Hugo Serrano Barbosa Filho, Marcos A. C. Oliveira, Ronaldo Menezes (2014)
  [doi](http://dx.doi.org/10.1007/978-3-319-15168-7_14)