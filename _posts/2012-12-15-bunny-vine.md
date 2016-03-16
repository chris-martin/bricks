---
layout: post
title: Bunny vine
date_granularity: month
thumbnail: /assets/posts/2012-12-15-bunny-vine/partial-thumb.png
---

This project for CS 6491 (computer graphics) uses Scala with JOGL to
generate a tree over approximately half of the faces of a triangle
manifold using a laced ring construction, and then render an animation
resembling a vine that grows upward along paths defined by the tree.

Links:
[Source](https://github.com/chris-martin/vine),
[animation](https://chris-martin.github.io/vine/animation.avi),
[paper](https://chris-martin.github.io/vine/paper.pdf),
[slides](https://chris-martin.github.io/vine/slides.pdf).

Mesh structure
--------------

The mesh is stored as a collection of components, where each component
is a collection of triangles, and each triangle consists of three corners.
The data comes from the Stanford Bunny read from a `ply` file.

Basic rendering with mesh edges drawn:

<div style="max-width: 400px; margin: 0 auto;">
    <img style="max-width: 100%"
         src="/assets/posts/2012-12-15-bunny-vine/mesh.png"/>
</div>

Each new triangle *t* added to the mesh initially belongs to its own
new component. If it is adjacent to another triangle *u*,
then the components of *t* and *u* are merged into a single component.
When two components merge, one of them may have its triangles reversed
to ensure consistent corner ordering among all of the triangles within
each component.

<div style="max-width: 400px; margin: 0 auto;">
    <img style="max-width: 100%"
         src="/assets/posts/2012-12-15-bunny-vine/flip.png"/>
</div>

When triangle C is added to this mesh, its component is merged with A
and then with B. A and B already have compatible ordering. B and C,
however, do not, indicated by the observation that their shared edges
point in the same direction. This is resolved by reversing the order
of the corners in C.

Triangle forest
---------------

The first task is to construct an undirected acyclic graph of triangles
(rendered as the darker color below). The graph is initialized with a triangle
located near the bottom-center of the model space, and is subsequently
expanded by conducting a traversal of the mesh, rejecting triangles
that share any edges with a triangle already in the graph.
A breadth-first strategy was chosen over depth-first to exhibit more vine-like behavior.

Demonstration of LR result:

<div style="max-width: 400px; margin: 0 auto;">
    <img style="max-width: 100%"
         src="/assets/posts/2012-12-15-bunny-vine/cycle.png"/>
</div>

The darker-colored surface indicates triangles that belong to the tree.
The resulting psuedo-Hamiltonian cycle is drawn in black.

<div style="max-width: 400px; margin: 0 auto;">
    <img style="max-width: 100%"
         src="/assets/posts/2012-12-15-bunny-vine/segment.png"/>
</div>

Segments of the vine are defined over
triplets of consecutive triangles.
For example, the sequence of triangles
(ABC, BCD, CDE)
corresponds to a vine segment from the midpoint
of BC to the midpoint of CD.

The bunny is slightly sunken into the mud puddle
so that its bottommost vertices are &ldquo;underground&rdquo;.
The triangle tree is split into a triangle directed forest,
where the underground nodes are used as the tree roots.
This ensures that all the vine segments sprouting from the
ground do so simultaneously at the beginning of the animation.

Vine rendering
--------------

The vine itself is rendered by using `gluCylinder` to construct each segment.
The thickness of each segment is manipulated to create a vine that is thicker
at its roots and grows over time. Thickness is calculated as
(*c<sub>1</sub> + c<sub>2</sub>* atan(*&alpha; + c<sub>3</sub>*) *+
c<sub>4</sub>* atan(*-&beta; + c<sub>5</sub>*)) *t*,
where the *c* are constants, *&alpha;* is the maximum distance from the node
to any leaf in the tree, *&beta;* is the distance from the node
to the root of the tree, and *t* is the time.

Screenshots from the animation:

<div style="max-width: 400px; margin: 0 auto;">
    <img style="max-width: 100%"
         src="/assets/posts/2012-12-15-bunny-vine/partial.png"/>
</div>

<div style="max-width: 400px; margin: 0 auto;">
    <img style="max-width: 100%"
         src="/assets/posts/2012-12-15-bunny-vine/full.png"/>
</div>
