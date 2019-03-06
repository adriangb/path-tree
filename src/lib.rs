use radix_tree::Node;
use std::iter::FromIterator;

#[derive(Clone, Debug)]
pub enum NodeKind {
    Root = 0,
    Static,
    Parameter,
    CatchAll,
}

#[derive(Clone, Debug)]
pub struct NodeMetadata<R> {
    key: bool,
    kind: NodeKind,
    data: Option<R>,
    params: Option<Vec<&'static str>>,
}

impl<R> NodeMetadata<R> {
    pub fn new() -> Self {
        NodeMetadata {
            key: false,
            data: None,
            params: None,
            kind: NodeKind::Root,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PathTree<R> {
    tree: Node<char, NodeMetadata<R>>,
}

impl<R> PathTree<R>
where
    R: Clone + Copy,
{
    pub fn new(path: &'static str, data: NodeMetadata<R>) -> Self {
        PathTree {
            tree: Node::new(path, data),
        }
    }

    pub fn insert(&mut self, path: &'static str, data: R) -> &mut Self {
        let mut node = &mut self.tree;
        let mut params: Option<Vec<Vec<char>>> = None;
        let mut buf: Vec<char> = path.trim_start_matches('/').chars().collect();

        // Root "/"
        if 0 == buf.len() {
            if let Some(d) = node.data.as_mut() {
                d.key = true;
                d.data = Some(data);
            }
            return self;
        }

        while 0 < buf.len() {
            let mut i: usize = 0;
            let mut next: Vec<char>;
            let mut meta = NodeMetadata::new();

            match buf[i] {
                '*' => {
                    next = buf.split_off(buf.len());
                    match params.as_mut() {
                        Some(p) => {
                            p.push(buf.split_off(1));
                        }
                        None => {
                            params.replace(vec![buf.split_off(1)]);
                        }
                    }
                    meta.kind = NodeKind::CatchAll;
                }
                ':' => {
                    next = buf.split_off(loop {
                        if i == buf.len() {
                            break i;
                        }

                        // Must be ":a.:b"
                        // Dont support ":a:b"
                        if i > 2 && (':' == buf[i] || '*' == buf[i]) {
                            break i - 1;
                        }

                        if '/' == buf[i] {
                            break i;
                        }
                        i += 1;
                    });
                    match params.as_mut() {
                        Some(p) => {
                            p.push(buf.split_off(1));
                        }
                        None => {
                            params.replace(vec![buf.split_off(1)]);
                        }
                    }
                    meta.kind = NodeKind::Parameter;
                }
                _ => {
                    next = buf.split_off(loop {
                        if i == buf.len() {
                            break i;
                        }
                        if ':' == buf[i] || '*' == buf[i] {
                            break i;
                        }
                        i += 1;
                    });
                    meta.kind = NodeKind::Static;
                }
            }

            let ended = 0 == next.len();

            // end
            if ended {
                if let Some(ref p) = params {
                    meta.params = Some(
                        p.iter()
                            .map(|x| {
                                &*(Box::leak(String::from_iter(x.into_iter()).into_boxed_str()))
                            })
                            .collect(),
                    );
                }
                meta.key = true;
                meta.data = Some(data);
            }

            // Add '/' ':' '*' to last
            node = node.add_node_with(&mut buf, Some(meta), 0, ended, |&l, &c, indices| {
                let mut j = l;
                if 0 == j {
                    return j;
                }

                if '*' == c {
                    return j;
                }
                if '*' == indices[j - 1] {
                    j -= 1;
                }

                if ':' == c {
                    return j;
                }
                if 0 < j && ':' == indices[j - 1] {
                    j -= 1;
                }

                if '/' == c {
                    return j;
                }
                if 0 < j && '/' == indices[j - 1] {
                    j -= 1;
                }

                j
            });

            buf = next;
        }

        self
    }

    pub fn find_with(
        &mut self,
        path: &'static str,
    ) -> Option<(&Node<char, NodeMetadata<R>>, Option<Vec<Vec<char>>>)> {
        recognize(&path.chars().collect(), &self.tree)
    }

    pub fn find(
        &mut self,
        path: &'static str,
    ) -> Option<(
        &Node<char, NodeMetadata<R>>,
        Option<Vec<(&'static str, &'static str)>>,
    )> {
        let mut params: Option<Vec<(&'static str, &'static str)>> = None;
        // Too many if and deep
        if let Some((node, values)) = &self.find_with(path) {
            if let Some(data) = &node.data {
                if !data.key {
                    return None;
                }

                if let Some(ps) = &data.params {
                    if let Some(vs) = &values {
                        params = Some(
                            vs.iter()
                                .enumerate()
                                .map(|(i, v)| {
                                    (
                                        &*ps[i],
                                        &*(Box::leak(
                                            String::from_iter(v.into_iter()).into_boxed_str(),
                                        )),
                                    )
                                })
                                .collect(),
                        );
                    }
                }
            }
            return Some((node, params));
        }

        None
    }
}

pub fn recognize<'a, R>(
    path: &Vec<char>,
    node: &'a Node<char, NodeMetadata<R>>,
) -> Option<(&'a Node<char, NodeMetadata<R>>, Option<Vec<Vec<char>>>)> {
    if 0 == path.len() {
        return None;
    }

    let mut buf: Vec<char> = path.clone();
    let mut values: Option<Vec<Vec<char>>> = None;

    match node.path[0] {
        '*' => {
            match values.as_mut() {
                Some(v) => {
                    v.push(buf);
                }
                None => {
                    values.replace(vec![buf]);
                }
            }
            return Some((&node, values));
        }
        ':' => {
            let mut n = 0;
            let l = node.indices.len();
            let t = l + 1;

            while n < t {
                let mut i = 0;
                let mut bf = buf.clone();
                let bl = bf.len();

                while i < bl {
                    if n < l {
                        if node.indices[n] == bf[0] {
                            i += 1;
                            while i < bl {
                                if node.indices[n] != bf[i] {
                                    i -= 1;
                                    break;
                                }
                                i += 1;
                            }
                            break;
                        } else if node.indices[n] == bf[i] {
                            break;
                        }
                    }
                    if '/' == bf[i] {
                        break;
                    }
                    i += 1;
                }

                if n == l && i == bl {
                    match values.as_mut() {
                        Some(v) => {
                            v.push(bf);
                        }
                        None => {
                            values.replace(vec![bf]);
                        }
                    }
                    return Some((&node, values));
                }

                if n < l && i < bl {
                    let next = bf.split_off(i);
                    if 0 < bf.len() {
                        let mut vs = values.clone();
                        match vs.as_mut() {
                            Some(v) => {
                                v.push(bf);
                            }
                            None => {
                                vs.replace(vec![bf]);
                            }
                        }
                        if let Some((n, v)) = recognize(&next, &node.nodes[n]) {
                            if let Some(mut d) = v {
                                match vs.as_mut() {
                                    Some(v) => {
                                        v.append(&mut d);
                                    }
                                    None => {
                                        vs.replace(d);
                                    }
                                }
                            }
                            return Some((&n, vs));
                        }
                    }
                }

                n += 1;
            }

            return None;
        }
        _ => {
            let mut m = buf.len();
            let mut n = m;
            let mut o = node.path.len();

            if m >= o {
                m = 0;
                while m < o && buf[m] == node.path[m] {
                    m += 1;
                }
            }

            if m < o {
                return None;
            }

            if m == o && m == n {
                return Some((&node, values));
            }

            let mut l = node.indices.len();
            if 0 == l {
                return None;
            }

            buf = buf.split_off(m);

            o = 0;
            let mut has_star = false;
            if '*' == node.indices[l - 1] {
                l -= 1;
                o = l;
                has_star = true;
            }

            n = 0;
            let mut has_colon = false;
            if 0 < l && ':' == node.indices[l - 1] {
                l -= 1;
                n = l;
                has_colon = true;
            }

            m = 0;
            let c = buf[m];
            let mut has_node = false;
            while m < l {
                if c == node.indices[m] {
                    has_node = true;
                    break;
                }
                m += 1;
            }

            // Static Node
            if has_node {
                if let Some((n, v)) = recognize(&buf, &node.nodes[m]) {
                    if let Some(mut d) = v {
                        match values.as_mut() {
                            Some(v) => {
                                v.append(&mut d);
                            }
                            None => {
                                values.replace(d);
                            }
                        }
                    }

                    // '/'
                    if '/' == n.path[n.path.len() - 1] {
                        if let Some(data) = &n.data {
                            if data.key {
                                // '/' is key node, ended
                                return Some((&n, values));
                            } else if 0 < n.indices.len() && '*' == n.indices[n.indices.len() - 1] {
                                // CatchAll '*'
                                return Some((&n.nodes[n.indices.len() - 1], values));
                            } else {
                                return None;
                            }
                        }
                    }

                    return Some((&n, values));
                }
            }

            // Parameter ':'
            if has_colon {
                if let Some((n, v)) = recognize(&buf, &node.nodes[n]) {
                    if let Some(mut d) = v {
                        match values.as_mut() {
                            Some(v) => {
                                v.append(&mut d);
                            }
                            None => {
                                values.replace(d);
                            }
                        }
                    }
                    return Some((&n, values));
                }
            }

            // CatchAll '*'
            if has_star {
                if let Some((n, v)) = recognize(&buf, &node.nodes[o]) {
                    if let Some(mut d) = v {
                        match values.as_mut() {
                            Some(v) => {
                                v.append(&mut d);
                            }
                            None => {
                                values.replace(d);
                            }
                        }
                    }
                    return Some((&n, values));
                }
            }

            // dbg!(buf);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_tree() {
        let mut tree = PathTree::<usize>::new("/", NodeMetadata::new());
        assert_eq!(tree.tree.path.len(), 1);

        tree.insert("/", 0);
        tree.insert("/users", 1);
        tree.insert("/users/:id", 2);
        tree.insert("/users/:id/:org", 3);
        tree.insert("/users/:user_id/repos", 4);
        tree.insert("/users/:user_id/repos/:id", 5);
        tree.insert("/users/:user_id/repos/:id/*any", 6);
        tree.insert("/:username", 7);
        tree.insert("/*any", 8);
        tree.insert("/about", 9);
        tree.insert("/about/", 10);
        tree.insert("/about/us", 11);
        tree.insert("/users/repos/*any", 12);

        // println!("{:#?}", tree);

        let node = tree.find("/");
        assert_eq!(node.is_some(), true);
        let res = node.unwrap();
        assert_eq!(res.0.path, ['/']);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 0);
        }
        assert_eq!(res.1, None);

        let node = tree.find("/users");
        assert_eq!(node.is_some(), true);
        let res = node.unwrap();
        assert_eq!(res.0.path, ['u', 's', 'e', 'r', 's']);
        assert_eq!(res.1, None);

        let node = tree.find("/about");
        assert_eq!(node.is_some(), true);
        let res = node.unwrap();
        assert_eq!(res.0.path, ['a', 'b', 'o', 'u', 't']);
        assert_eq!(res.1, None);

        let node = tree.find("/about/");
        assert_eq!(node.is_some(), true);
        let res = node.unwrap();
        assert_eq!(res.0.path, ['/']);
        assert_eq!(res.1, None);

        let node = tree.find("/about/us");
        assert_eq!(node.is_some(), true);
        let res = node.unwrap();
        assert_eq!(res.0.path, ['u', 's']);
        assert_eq!(res.1, None);

        let node = tree.find("/username");
        assert_eq!(node.is_some(), true);
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 7);
        }
        assert_eq!(res.1.unwrap(), [("username", "username")]);

        let node = tree.find("/user/s");
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 8); // Data
        }
        assert_eq!(res.1.unwrap(), [("any", "user/s")]);

        let node = tree.find("/users/fundon/repo");
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 3);
        }
        assert_eq!(res.1.unwrap(), [("id", "fundon"), ("org", "repo")]);

        let node = tree.find("/users/fundon/repos");
        let res = node.unwrap();
        assert_eq!(res.0.path, "repos".chars().collect::<Vec<char>>());
        assert_eq!(res.1.unwrap(), [("user_id", "fundon")]);

        let node = tree.find("/users/fundon/repos/trek-rs");
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 5); // Data
        }
        assert_eq!(res.1.unwrap(), [("user_id", "fundon"), ("id", "trek-rs"),]);

        let node = tree.find("/users/fundon/repos/trek-rs/");
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.1.unwrap(), [("user_id", "fundon"), ("id", "trek-rs"),]);

        let node = tree.find("/users/fundon/repos/trek-rs/noder");
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(
            res.1.unwrap(),
            [("user_id", "fundon"), ("id", "trek-rs"), ("any", "noder"),]
        );

        let node = tree.find("/users/fundon/repos/trek-rs/noder/issues");
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 6); // Data
        }
        assert_eq!(
            res.1.unwrap(),
            [
                ("user_id", "fundon"),
                ("id", "trek-rs"),
                ("any", "noder/issues"),
            ]
        );

        let node = tree.find("/users/repos/");
        let res = node.unwrap();
        assert_eq!(res.0.path, "*".chars().collect::<Vec<char>>());
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 12); // Data
        }
        assert_eq!(res.1.is_none(), true);

        let node = tree.find("/about/as");
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.1.unwrap(), [("any", "about/as")]);
    }

    #[test]
    fn statics() {
        let mut tree = PathTree::new("/", NodeMetadata::new());
        let nodes = [
            "/hi",
            "/contact",
            "/co",
            "/c",
            "/a",
            "/ab",
            "/doc/",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/α",
            "/β",
        ];
        let mut i = 0;
        for node in &nodes {
            tree.insert(node, i);
            i += 1;
        }

        // println!("tree {:#?}", tree);

        let node = tree.find("/a");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['a']);

        let node = tree.find("/");
        assert!(node.is_none());

        let node = tree.find("/hi");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['h', 'i']);

        let node = tree.find("/contact");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, "ntact".chars().collect::<Vec<char>>());

        let node = tree.find("/co");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, "o".chars().collect::<Vec<char>>());

        let node = tree.find("/con");
        assert!(node.is_none());

        let node = tree.find("/cona");
        assert!(node.is_none());

        let node = tree.find("/no");
        assert!(node.is_none());

        let node = tree.find("/ab");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['b']);

        let node = tree.find("/α");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['α']);

        let node = tree.find("/β");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['β']);
    }

    #[test]
    fn wildcards() {
        let mut tree = PathTree::new("/", NodeMetadata::new());
        let nodes = [
            "/",
            "/cmd/:tool/:sub",
            "/cmd/:tool/",
            "/cmd/vet",
            "/src/*filepath",
            "/src1/",
            "/src1/*filepath",
            "/src2*filepath",
            "/search/",
            "/search/:query",
            "/search/invalid",
            "/user_:name",
            "/user_:name/about",
            "/user_x",
            "/files/:dir/*filepath",
            "/doc/",
            "/doc/rust_faq.html",
            "/doc/rust1.html",
            "/info/:user/public",
            "/info/:user/project/:project",
        ];
        let mut i = 0;
        for node in &nodes {
            tree.insert(node, i);
            i += 1;
        }

        // println!("tree {:#?}", tree);

        let node = tree.find("/");
        assert!(node.is_some());

        let node = tree.find("/cmd/test/");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['/']);
        assert_eq!(res.1.unwrap(), [("tool", "test")]);

        let node = tree.find("/cmd/test");
        assert!(node.is_none());

        let node = tree.find("/cmd/test/3");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, [':']);
        assert_eq!(res.1.unwrap(), [("tool", "test"), ("sub", "3")]);

        let node = tree.find("/src/");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['*']);
        assert_eq!(res.1, None);

        let node = tree.find("/src/some/file.png");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['*']);
        assert_eq!(res.1.unwrap(), [("filepath", "some/file.png")]);

        let node = tree.find("/search/");
        assert!(node.is_some());

        let node = tree.find("/search/someth!ng+in+ünìcodé");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, [':']);
        assert_eq!(res.1.unwrap(), [("query", "someth!ng+in+ünìcodé")]);

        let node = tree.find("/search/someth!ng+in+ünìcodé/");
        assert!(node.is_none());

        let node = tree.find("/user_rust");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, [':']);
        assert_eq!(res.1.unwrap(), [("name", "rust")]);

        let node = tree.find("/user_rust/about");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, "/about".chars().collect::<Vec<char>>());
        assert_eq!(res.1.unwrap(), [("name", "rust")]);

        let node = tree.find("/files/js/inc/framework.js");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, ['*']);
        assert_eq!(
            res.1.unwrap(),
            [("dir", "js"), ("filepath", "inc/framework.js")]
        );

        let node = tree.find("/info/gordon/public");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, "ublic".chars().collect::<Vec<char>>());
        assert_eq!(res.1.unwrap(), [("user", "gordon")]);

        let node = tree.find("/info/gordon/project/rust");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, [':']);
        assert_eq!(res.1.unwrap(), [("user", "gordon"), ("project", "rust")]);
    }

    #[test]
    fn single_named_parameter() {
        //  Pattern: /users/:id
        //
        //      /users/gordon              match
        //      /users/you                 match
        //      /users/gordon/profile      no match
        //      /users/                    no match
        let mut tree = PathTree::new("/", NodeMetadata::new());

        tree.insert("/users/:id", 0);

        // println!("tree {:#?}", tree);

        let node = tree.find("/users/");
        assert!(node.is_none());

        let node = tree.find("/users/gordon/profile");
        assert!(node.is_none());

        let node = tree.find("/users/gordon");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, [':']);
        if let Some(data) = &node.data {
            assert_eq!(data.key, true);
        }
        assert_eq!(res.1.unwrap(), [("id", "gordon")]);

        let node = tree.find("/users/you");
        assert!(node.is_some());
        let res = node.unwrap();
        let node = &res.0;
        assert_eq!(node.path, [':']);
        if let Some(data) = &node.data {
            assert_eq!(data.key, true);
        }
        assert_eq!(res.1.unwrap(), [("id", "you")]);
    }

    #[test]
    fn static_and_named_parameter() {
        //  Pattern: /a/b/c
        //  Pattern: /a/c/d
        //  Pattern: /a/c/a
        //  Pattern: /:id/c/e
        //
        //      /a/b/c                  match
        //      /a/c/d                  match
        //      /a/c/a                  match
        //      /a/c/e                  match
        let mut tree = PathTree::new("/", NodeMetadata::new());

        tree.insert("/a/b/c", "/a/b/c");
        tree.insert("/a/c/d", "/a/c/d");
        tree.insert("/a/c/a", "/a/c/a");
        tree.insert("/:id/c/e", "/:id/c/e");

        // println!("tree {:#?}", tree);

        let node = tree.find("/");
        assert!(node.is_none());

        let node = tree.find("/a/b/c");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['b', '/', 'c']);
        assert_eq!(res.1, None);

        let node = tree.find("/a/c/d");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['d']);
        assert_eq!(res.1, None);

        let node = tree.find("/a/c/a");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['a']);
        assert_eq!(res.1, None);

        let node = tree.find("/a/c/e");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['/', 'c', '/', 'e']);
        assert_eq!(res.1.unwrap(), [("id", "a")]);
    }

    #[test]
    fn multi_named_parameters() {
        //  Pattern: /:lang/:keyword
        //  Pattern: /:id
        //
        //      /rust                     match
        //      /rust/let                 match
        //      /rust/let/const           no match
        //      /rust/let/                no match
        //      /rust/                    no match
        //      /                         no match
        let mut tree = PathTree::new("/", NodeMetadata::new());

        tree.insert("/:lang/:keyword", true);
        tree.insert("/:id", true);
        // tree.insert("/:id/:post_id", NodeMetadata::new());

        // println!("tree {:#?}", tree);

        let node = tree.find("/");
        assert!(node.is_none());

        let node = tree.find("/rust/");
        assert!(node.is_none());

        let node = tree.find("/rust/let/");
        assert!(node.is_none());

        let node = tree.find("/rust/let/const");
        assert!(node.is_none());

        let node = tree.find("/rust/let");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.1.unwrap(), [("lang", "rust"), ("keyword", "let")]);

        let node = tree.find("/rust");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.1.unwrap(), [("id", "rust")]);
    }

    #[test]
    fn catch_all_parameter() {
        //  Pattern: /src/*filepath
        //
        //      /src                      no match
        //      /src/                     match
        //      /src/somefile.go          match
        //      /src/subdir/somefile.go   match
        let mut tree = PathTree::new("/", NodeMetadata::new());

        tree.insert("/src/*filepath", "* files");

        let node = tree.find("/src");
        assert!(node.is_none());

        let node = tree.find("/src/");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert!(res.1.is_none());

        let node = tree.find("/src/somefile.rs");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.1.unwrap(), [("filepath", "somefile.rs")]);

        let node = tree.find("/src/subdir/somefile.rs");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.1.unwrap(), [("filepath", "subdir/somefile.rs")]);

        let node = tree.find("/src.rs");
        assert!(node.is_none());

        let node = tree.find("/rust");
        assert!(node.is_none());

        // split node, 'src/' is key node
        tree.insert("/src/", "dir");

        let node = tree.find("/src/");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, "src/".chars().collect::<Vec<char>>());
        assert!(res.1.is_none());
    }

    #[test]
    fn static_and_catch_all_parameter() {
        //  Pattern: /a/b/c
        //  Pattern: /a/c/d
        //  Pattern: /a/c/a
        //  Pattern: /a/*c
        //
        //      /a/b/c                  match
        //      /a/c/d                  match
        //      /a/c/a                  match
        //      /a/c/e                  match
        let mut tree = PathTree::new("/", NodeMetadata::new());

        tree.insert("/a/b/c", "/a/b/c");
        tree.insert("/a/c/d", "/a/c/d");
        tree.insert("/a/c/a", "/a/c/a");
        tree.insert("/a/*c", "/a/*c");

        // println!("tree {:#?}", tree);

        let node = tree.find("/");
        assert!(node.is_none());

        let node = tree.find("/a/b/c");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['b', '/', 'c']);
        assert_eq!(res.1, None);

        let node = tree.find("/a/c/d");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['d']);
        assert_eq!(res.1, None);

        let node = tree.find("/a/c/a");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['a']);
        assert_eq!(res.1, None);

        let node = tree.find("/a/c/e");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.1.unwrap(), [("c", "c/e")]);
    }

    #[test]
    fn root_catch_all_parameter() {
        //  Pattern: /
        //  Pattern: /*
        //  Pattern: /users/*
        //
        //      /                  match *
        //      /download          match *
        //      /users/fundon      match users *
        let mut tree = PathTree::<fn() -> usize>::new("/", NodeMetadata::new());

        tree.insert("/", || 1);
        tree.insert("/*", || 2);
        tree.insert("/users/*", || 3);

        // println!("tree {:#?}", tree);

        let node = tree.find("/");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['/']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap()(), 1);
        }

        let node = tree.find("/download");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap()(), 2);
        }
        assert_eq!(res.1.unwrap(), [("", "download")]);

        let node = tree.find("/users/fundon");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap()(), 3);
        }
        assert_eq!(res.1.unwrap(), [("", "fundon")]);
    }

    #[test]
    fn multi_named_parameters_in_one_segment() {
        // Pattern: /:name.:ext
        let mut tree = PathTree::<usize>::new("/", NodeMetadata::new());

        tree.insert("/:name", 0);
        tree.insert("/:name.:ext", 1);
        tree.insert("/:name.:a0.:b0", 3);
        tree.insert("/:name.:a0-:b1", 4);
        tree.insert("/:name-:a1-:b1", 5);
        tree.insert("/:name-*any", 2);
        tree.insert("/:name/:age", 6);

        // println!("tee: {:#?}", tree);

        let node = tree.find("/main");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 0);
        }
        assert_eq!(res.1.unwrap(), [("name", "main")]);

        let node = tree.find("/main.rs");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 1);
        }
        assert_eq!(res.1.unwrap(), [("name", "main"), ("ext", "rs")]);

        let node = tree.find("/main.rs-build");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 4);
        }
        assert_eq!(
            res.1.unwrap(),
            [("name", "main"), ("a0", "rs"), ("b1", "build")]
        );

        let node = tree.find("/main.rs.build");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 3);
        }
        assert_eq!(
            res.1.unwrap(),
            [("name", "main"), ("a0", "rs"), ("b0", "build")]
        );

        let node = tree.find("/main-rs-build");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 5);
        }
        assert_eq!(
            res.1.unwrap(),
            [("name", "main"), ("a1", "rs"), ("b1", "build")]
        );

        let node = tree.find("/main-rs-build/other");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, ['*']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 2);
        }
        assert_eq!(
            res.1.unwrap(),
            [("name", "main"), ("any", "rs-build/other")]
        );

        let node = tree.find("/main/rs");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 6);
        }
        assert_eq!(res.1.unwrap(), [("name", "main"), ("age", "rs")]);
    }

    #[test]
    fn git_compare() {
        // Pattern: ":rev_a...:rev_b"
        let mut tree = PathTree::<usize>::new("/", NodeMetadata::new());

        tree.insert("/:rev_a.:dot.:rev_b", 0);

        // println!("tee: {:#?}", tree);

        let node = tree.find("/master...dev");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 0);
        }
        assert_eq!(
            res.1.unwrap(),
            [("rev_a", "master"), ("dot", "."), ("rev_b", "dev")]
        );

        let node = tree.find("/master.a.dev");
        assert!(node.is_some());
        let res = node.unwrap();
        assert_eq!(res.0.path, [':']);
        assert_eq!(res.0.data.is_some(), true);
        if let Some(meta) = &res.0.data {
            assert_eq!(meta.data.unwrap(), 0);
        }
        assert_eq!(
            res.1.unwrap(),
            [("rev_a", "master"), ("dot", "a"), ("rev_b", "dev")]
        );
    }
}
