//! path-tree is a lightweight high performance HTTP request router for Rust.

#![deny(unsafe_code)]
#![warn(
    nonstandard_style,
    rust_2018_idioms,
    future_incompatible,
    missing_debug_implementations
)]

use std::mem;

#[derive(Clone, Debug)]
pub enum NodeKind {
    Static(Vec<u8>),
    Parameter,
    CatchAll,
}

#[derive(Clone, Debug)]
pub struct Node<T> {
    kind: NodeKind,
    data: Option<T>,
    nodes: Option<Vec<Self>>,
    indices: Option<Vec<u8>>,
    params: Option<Vec<Vec<u8>>>,
}

impl<T> Default for Node<T> {
    fn default() -> Self {
        Self::new(NodeKind::Static(Vec::new()))
    }
}

impl<T> Node<T> {
    pub fn new(kind: NodeKind) -> Self {
        Self {
            kind,
            data: None,
            nodes: None,
            params: None,
            indices: None,
        }
    }

    fn add_node(&mut self, c: u8, kind: NodeKind) -> &mut Self {
        let indices: &mut Vec<u8> = self.indices.get_or_insert_with(Vec::new);
        let nodes: &mut Vec<Node<T>> = self.nodes.get_or_insert_with(Vec::new);

        match position(indices, c) {
            Some(i) => match kind {
                NodeKind::Static(ref s) => nodes[i].insert(s),
                _ => &mut nodes[i],
            },
            None => {
                indices.push(c);
                nodes.push(Node::new(kind));
                nodes.last_mut().unwrap()
            }
        }
    }

    pub fn add_node_static(&mut self, p: &[u8]) -> &mut Self {
        if let Some(c) = p.iter().next() {
            self.add_node(*c, NodeKind::Static(p.to_vec()))
        } else {
            self
        }
    }

    pub fn add_node_dynamic(&mut self, c: u8, kind: NodeKind) -> &mut Self {
        self.add_node(c, kind)
    }

    pub fn insert(&mut self, p: &[u8]) -> &mut Self {
        match self.kind {
            NodeKind::Static(ref mut s) if s.len() == 0 => {
                // *s += p;
                s.extend_from_slice(p);
                self
            }
            NodeKind::Static(ref mut s) => {
                let np = loc(s, p);
                let l = np.len();

                // Split node
                if l < s.len() {
                    *s = s[l..].to_owned();
                    let mut node = Node {
                        data: None,
                        params: None,
                        nodes: Some(Vec::new()),
                        indices: s.iter().next().map(|c| [*c].to_vec()),
                        kind: NodeKind::Static(np.to_owned()),
                    };
                    mem::swap(self, &mut node);
                    self.nodes.as_mut().unwrap().push(node);
                }

                if l == p.len() {
                    self
                } else {
                    self.add_node_static(&p[l..])
                }
            }
            NodeKind::Parameter => self.add_node_static(p),
            NodeKind::CatchAll => self,
        }
    }

    pub fn find<'a>(&'a self, mut p: &'a [u8]) -> Option<(&'a Self, Vec<&'a [u8]>)> {
        let mut params = Vec::new();

        match self.kind {
            NodeKind::Static(ref s) => {
                let np = loc(s, p);
                let l = np.len();

                if l == 0 {
                    None
                } else if l < s.len() {
                    None
                } else if l == s.len() && l == p.len() {
                    Some((
                        // Fixed: has only route `/*`
                        // Ended `/` `/*any`
                        if self.data.is_none()
                            && self.indices.is_some()
                            && b'/' == *s.iter().last().unwrap()
                        {
                            &self.nodes.as_ref().unwrap()
                                [position(self.indices.as_ref().unwrap(), b'*')?]
                        } else {
                            self
                        },
                        params,
                    ))
                } else {
                    let indices = self.indices.as_ref()?;
                    let nodes = self.nodes.as_ref().unwrap();

                    p = &p[l..];

                    // Static
                    if let Some(i) = position(indices, *p.iter().next().unwrap()) {
                        if let Some((n, ps)) = nodes[i].find(p).as_mut() {
                            params.append(ps);

                            return Some((
                                // Ended `/` `/*any`
                                match &n.kind {
                                    NodeKind::Static(s)
                                        if n.data.is_none()
                                            && n.indices.is_some()
                                            && b'/' == *s.iter().last().unwrap() =>
                                    {
                                        &n.nodes.as_ref().unwrap()
                                            [position(n.indices.as_ref().unwrap(), b'*')?]
                                    }
                                    _ => n,
                                },
                                params,
                            ));
                        }
                    }

                    // Named Parameter
                    if let Some(i) = position(indices, b':') {
                        if let Some((n, ps)) = nodes[i].find(p).as_mut() {
                            params.append(ps);
                            return Some((n, params));
                        }
                    }

                    // Catch-All Parameter
                    if let Some(i) = position(indices, b'*') {
                        if let Some((n, ps)) = nodes[i].find(p).as_mut() {
                            params.append(ps);
                            return Some((n, params));
                        }
                    }

                    None
                }
            }
            NodeKind::Parameter => match position(p, b'/') {
                Some(i) => {
                    let indices = self.indices.as_ref()?;

                    params.push(&p[..i]);
                    p = &p[i..];

                    let (n, ref mut ps) = self.nodes.as_ref().unwrap()
                        [position(indices, p.iter().next().cloned().unwrap())?]
                    .find(p)?;

                    params.append(ps);
                    Some((n, params))
                }
                None => {
                    params.push(p);
                    Some((self, params))
                }
            },
            NodeKind::CatchAll => {
                params.push(p);
                Some((self, params))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct PathTree<T>(Node<T>);

impl<T> Default for PathTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> PathTree<T> {
    pub fn new() -> Self {
        Self(Node::new(NodeKind::Static([47].to_vec())))
    }

    pub fn insert(&mut self, path: &str, data: T) -> &mut Self {
        let mut next = true;
        let mut node = &mut self.0;
        let mut params: Option<Vec<Vec<u8>>> = None;
        let mut path = path.trim_start_matches('/').as_bytes();

        if path.len() == 0 {
            node.data = Some(data);
            return self;
        }

        while next {
            match path.iter().position(has_colon_or_star) {
                Some(i) => {
                    let kind: NodeKind;
                    let mut prefix = &path[..i];
                    let mut suffix = &path[i..];

                    if prefix.len() > 0 {
                        node = node.add_node_static(prefix);
                    }

                    prefix = &suffix[..1];
                    suffix = &suffix[1..];

                    let c = prefix.iter().next().cloned().unwrap();
                    if c == b':' {
                        match suffix.iter().position(has_star_or_slash) {
                            Some(i) => {
                                path = &suffix[i..];
                                suffix = &suffix[..i];
                            }
                            None => {
                                next = false;
                            }
                        }
                        kind = NodeKind::Parameter;
                    } else {
                        next = false;
                        kind = NodeKind::CatchAll;
                    }
                    params.get_or_insert_with(Vec::new).push(suffix.to_vec());
                    node = node.add_node_dynamic(c, kind);
                }
                None => {
                    next = false;
                    node = node.add_node_static(path);
                }
            }
        }

        node.data = Some(data);
        node.params = params;

        self
    }

    pub fn find_as_bytes<'a>(
        &'a self,
        path: &'a [u8],
    ) -> Option<(&'a T, Vec<(&'a [u8], &'a [u8])>)> {
        match self.0.find(path) {
            Some((node, values)) => match (node.data.as_ref(), node.params.as_ref()) {
                (Some(data), Some(params)) => Some((
                    data,
                    params
                        .iter()
                        .zip(values.iter())
                        .map(|(a, b)| (a.as_slice(), *b))
                        .collect(),
                )),
                (Some(data), None) => Some((data, Vec::new())),
                _ => None,
            },
            None => None,
        }
    }

    pub fn find<'a>(&'a self, path: &'a str) -> Option<(&'a T, Vec<(String, String)>)> {
        self.find_as_bytes(path.as_bytes()).map(|(n, v)| {
            (
                n,
                v.iter()
                    .map(|(a, b)| {
                        (
                            String::from_utf8(a.to_vec()).unwrap(),
                            String::from_utf8(b.to_vec()).unwrap(),
                        )
                    })
                    .collect(),
            )
        })
    }
}

#[inline]
fn has_colon_or_star(c: &u8) -> bool {
    (*c == b':') | (*c == b'*')
}

#[inline]
fn has_star_or_slash(c: &u8) -> bool {
    (*c == b'*') | (*c == b'/')
}

#[inline]
fn position(p: &[u8], c: u8) -> Option<usize> {
    p.iter().position(|x| *x == c)
}

#[inline]
fn loc(s: &[u8], p: &[u8]) -> Vec<u8> {
    s.iter()
        .zip(p.iter())
        .take_while(|(a, b)| a == b)
        .map(|v| *v.0)
        .collect()
}
