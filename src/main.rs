use tree_sitter::{Parser, Node, Tree};
use tree_sitter_openscad as ts_openscad; // Make sure this crate is in Cargo.toml

use csgrs::CSG;
use csgrs::Axis; // for mirror if you want it

use nalgebra::Vector3;

fn main() {
    let openscad_source = r#"
        // Example OpenSCAD code
        union() {
          translate([10, 0, 0]) cube([2,2,2]);
          difference() {
            sphere(r=3);
            translate([0,0,1]) cylinder(h=3, r=1);
          }
        }
    "#;

    // 1) Parse the code
    let csg = parse_openscad_code(openscad_source)
        .unwrap_or_else(|| {
            eprintln!("Failed to parse or interpret the OpenSCAD code.");
            // Return an empty CSG if error:
            CSG::new()
        });

    // 2) Do something with the resulting CSG, e.g. write out STL:
    let stl_data = csg.to_stl("converted_solid");
    println!("{}", stl_data);
}

/// Parse an OpenSCAD source string into a csgrs `CSG` object.
/// Returns `None` if parsing fails or if we can’t interpret the AST.
pub fn parse_openscad_code(code: &str) -> Option<CSG> {
    // Initialize the parser
    let mut parser = Parser::new();
    parser
        .set_language(ts_openscad::language())
        .expect("Error loading tree-sitter OpenSCAD grammar");

    let tree = parser.parse(code, None)?;
    let root_node = tree.root_node();

    // Recursively evaluate the root node: 
    //   typically it's a 'source_file' node containing many child statements.
    let csg = eval_node(root_node, code).unwrap_or_else(CSG::new);
    Some(csg)
}

/// Recursively evaluate a single `Node` in the parse tree.
/// We return a `Some(CSG)` if we can interpret this node as geometry,
/// or `None` if not recognized / not geometry.
fn eval_node(node: Node, code: &str) -> Option<CSG> {
    let kind = node.kind();

    match kind {
        // The top-level node is usually "source_file".
        // We'll gather all geometry from its children and union them.
        "source_file" => {
            let mut result = CSG::new();
            for child in node.children(&mut node.walk()) {
                if let Some(child_csg) = eval_node(child, code) {
                    // We union them at top-level:
                    result = result.union(&child_csg);
                }
            }
            Some(result)
        }

        // Some items are statements or module calls, etc.
        "_statement" => {
            // Usually _statement is a wrapper that has a single interesting child
            // like "modifier_chain", "transform_chain", "union_block", etc.
            // So we look at children.
            for child in node.children(&mut node.walk()) {
                if let Some(csg) = eval_node(child, code) {
                    return Some(csg);
                }
            }
            None
        }

        // e.g. "union_block" => { ... }
        "union_block" => {
            // In the official grammar, union_block => '{' repeat(_item) '}'
            // Let’s gather all `_item`s and union them.
            let mut result = CSG::new();
            for child in node.children(&mut node.walk()) {
                if child.kind() == "_item" {
                    if let Some(item_csg) = eval_node(child, code) {
                        result = result.union(&item_csg);
                    }
                }
            }
            Some(result)
        }

        // The grammar doesn’t have explicit "difference"/"intersection" nodes,
        // so we often see them as a `module_call` with an identifier "difference"/"intersection"
        // wrapping a union_block or curly block. We can handle it in "module_call".
        
        // A transform_chain is typically `module_call some_statement`.
        "transform_chain" => {
            // transform_chain = (module_call) (_statement)
            let mut child_nodes = node.children(&mut node.walk()).collect::<Vec<_>>();

            // The first child is presumably the module_call
            // The second child is presumably the next statement or block
            if child_nodes.len() < 2 {
                return None;
            }

            let module_call_node = child_nodes.remove(0);
            let body_node = child_nodes.remove(0);

            // Evaluate the module_call itself (which might be a transform)
            // That might return a geometry or a "dummy" indicating "we need to apply something to the body"
            let call_csg = eval_node(module_call_node, code);

            // Evaluate the body (the shape to transform)
            let body_csg = eval_node(body_node, code).unwrap_or_else(CSG::new);

            // If `module_call` was a transform like `translate([x,y,z])`,
            // we’ll apply that transform to the body. 
            // If `module_call` was `union()`, we interpret differently.
            // So we actually do the interpret in `eval_module_call`.
            // But for simple layering, let's do a fallback here:
            if let Some(transformed_body) = call_csg {
                // In some patterns, we might want to combine them.
                // But typically, if call_csg is just "some shape," we’d union it with body?
                // Actually in typical usage, the transform node is a function that returns
                // "body_csg transformed." Let's interpret it that way inside `eval_module_call`.
                // So maybe we just return `body_csg` if `call_csg` is None.
                // Or return union if we want. But the usual approach is:
                //   let final_csg = apply_transform(trans_params, body_csg).
                // That "transform" is discovered in `eval_module_call`.

                // Because we don’t have a way to store partial states,
                // we treat `call_csg` as the final shape. Or we might guess the user wanted 
                // "transformed body." The simpler approach is to do it all in `eval_module_call`.
                return Some(transformed_body);
            } else {
                // If the module call is recognized as a transform, 
                // then `eval_module_call` will have already applied it to `body_csg`.
                // In that case, we might have stashed the result in an internal global 
                // or we simply do the transform logic in that function. 
                // For simplicity, let's just interpret the module_call’s name 
                // (like "translate", "rotate") and do everything there:
                // Actually let's handle it: 
                None
            }
        }

        // "module_call" => e.g. `translate([x,y,z])`, `rotate(...)`, `scale(...)`,
        //  or shapes like `cube(...)`, or even "union()", "difference()", "intersection()".
        "module_call" => eval_module_call(node, code),

        // An _item could be an assignment or a statement or a module_declaration, etc.
        "_item" => {
            // e.g. `_item: (assignment ';') | _statement | module_declaration | function_declaration`
            // We only care about geometry statements. So we look at children again:
            for child in node.children(&mut node.walk()) {
                if let Some(csg) = eval_node(child, code) {
                    return Some(csg);
                }
            }
            None
        }

        // If none of the above, walk children to see if they produce geometry:
        _ => {
            let mut result: Option<CSG> = None;
            for child in node.children(&mut node.walk()) {
                if let Some(csg) = eval_node(child, code) {
                    result = Some(match result {
                        Some(existing) => existing.union(&csg),
                        None => csg,
                    });
                }
            }
            result
        }
    }
}

/// Interpret a `module_call` node.  
/// This is where we catch OpenSCAD calls like `cube(...)`, `sphere(r=...)`, `translate(...)` etc.
fn eval_module_call(node: Node, code: &str) -> Option<CSG> {
    // We expect the first child to be `identifier` (the module name),
    // and the second child to be `arguments`.
    let mut children = node.children(&mut node.walk()).collect::<Vec<_>>();

    if children.is_empty() {
        return None;
    }

    // The first child is the identifier
    let ident_node = children.remove(0);
    if ident_node.kind() != "identifier" {
        return None;
    }
    let ident_str = ident_node.utf8_text(code.as_bytes()).ok()?.to_string();

    // The second child (if present) is `arguments(...)`
    let mut args_csg = CSG::new();
    let mut arg_values = vec![]; // We'll store numeric parameters, etc.

    let mut body_csg = None; // In case we also handle block after arguments

    // See if there's an `arguments` node:
    if !children.is_empty() && children[0].kind() == "arguments" {
        let args_node = children.remove(0);
        arg_values = parse_arguments(args_node, code);
    }

    // The next child could be a statement or block if it's a transform with a body
    if !children.is_empty() {
        // Typically, the next child might be `_statement` or something containing geometry
        body_csg = eval_node(children.remove(0), code);
    }

    // Now interpret the identifier:
    match ident_str.as_str() {
        // -------------- 3D Primitives --------------
        "cube" => {
            // In real OpenSCAD: `cube(size = [sx, sy, sz], center = true/false)`
            // We have arg_values parsed in some naive way. Let’s just look for a float array or fallback:
            let (sx, sy, sz, center) = match extract_cube_params(&arg_values) {
                Some((x, y, z, c)) => (x, y, z, c),
                None => (1.0, 1.0, 1.0, false),
            };
            let csg_prim = if center {
                // center => shift by half
                let half = [sx / 2.0, sy / 2.0, sz / 2.0];
                CSG::cube(Some((&[0.0, 0.0, 0.0], &half))) // radius form
            } else {
                // non-centered => same radius approach, but the corner is at origin
                // or just interpret as radius = [sx/2, sy/2, sz/2], and then shift the center
                let csg_temp = CSG::cube(Some((&[0.0, 0.0, 0.0], &[sx / 2.0, sy / 2.0, sz / 2.0])));
                // translate it so the min corner is at (0,0,0).
                csg_temp.translate(Vector3::new(sx / 2.0, sy / 2.0, sz / 2.0))
            };

            // If there's a block after the `cube(...)`, 
            // that block presumably is child geometry. In regular OpenSCAD, `cube(...)` can't have a child block.
            // But let's see if user wrote it anyway. We can union them or ignore them. We'll union for demonstration:
            if let Some(body) = body_csg {
                return Some(csg_prim.union(&body));
            } else {
                return Some(csg_prim);
            }
        }

        "sphere" => {
            // Typically `sphere(r=...)` or `sphere(d=...)` or `sphere(...)`.
            // Let’s just read the first argument as radius if we see it:
            let radius = arg_values.get(0).cloned().unwrap_or(1.0);
            let csg_prim = CSG::sphere(Some((&[0.0, 0.0, 0.0], radius, 16, 8)));
            if let Some(body) = body_csg {
                Some(csg_prim.union(&body))
            } else {
                Some(csg_prim)
            }
        }

        "cylinder" => {
            // Typically `cylinder(h=..., r=..., center=...)`.
            // For simplicity, read h from arg[0], r from arg[1].
            let h = arg_values.get(0).cloned().unwrap_or(1.0);
            let r = arg_values.get(1).cloned().unwrap_or(1.0);
            let csg_prim = CSG::cylinder(Some((&[0.0, 0.0, 0.0], &[0.0, h, 0.0], r, 16)));
            if let Some(body) = body_csg {
                Some(csg_prim.union(&body))
            } else {
                Some(csg_prim)
            }
        }

        // -------------- 2D Primitives --------------
        "square" => {
            // In real OpenSCAD: `square(size=[w,h], center=...)`.
            // We just handle a single numeric => w, or two => [w,h].
            let (w, h, center) = match extract_square_params(&arg_values) {
                Some((ww, hh, c)) => (ww, hh, c),
                None => (1.0, 1.0, false),
            };
            let csg_prim = CSG::square(Some(([w, h], center)));
            if let Some(body) = body_csg {
                Some(csg_prim.union(&body))
            } else {
                Some(csg_prim)
            }
        }

        "circle" => {
            // `circle(r=..., fn=...)`.
            let r = arg_values.get(0).cloned().unwrap_or(1.0);
            let csg_prim = CSG::circle(Some((r, 32)));
            if let Some(body) = body_csg {
                Some(csg_prim.union(&body))
            } else {
                Some(csg_prim)
            }
        }

        // -------------- Boolean ops --------------
        "union" => {
            // Usually we interpret the body’s geometry. 
            // If the body is a "union_block" we already processed the block union.
            // So simply return that. 
            // If there's no body, we return an empty shape. 
            Some(body_csg.unwrap_or_else(CSG::new))
        }

        "difference" => {
            // difference() { A(); B(); } means A - B - ...
            // In typical OpenSCAD, the block might contain multiple sub-objects, 
            // the first is the base, the rest are subtracted in turn. 
            // Our minimal approach: if the body node is a union_block or so,
            // we can parse all its children into a list [A, B, C...] 
            // then do A - B - C ...
            // For simplicity, let's just parse them into a union, 
            // then do (first - second - third...).  

            let items = collect_child_csg(body_csg.unwrap_or_else(CSG::new));
            if items.is_empty() {
                return Some(CSG::new());
            }
            let mut result = items[0].clone();
            for i in 1..items.len() {
                result = result.subtract(&items[i]);
            }
            Some(result)
        }

        "intersection" => {
            // intersection() { A(); B(); ... } => A ∩ B ∩ ...
            let items = collect_child_csg(body_csg.unwrap_or_else(CSG::new));
            if items.is_empty() {
                return Some(CSG::new());
            }
            let mut result = items[0].clone();
            for i in 1..items.len() {
                result = result.intersect(&items[i]);
            }
            Some(result)
        }

        // -------------- Transforms --------------
        "translate" => {
            // e.g. `translate([x,y,z]) { object(); }`
            // We'll parse the first argument as a 3-vector
            let (tx, ty, tz) = match extract_vec3(&arg_values) {
                Some((x, y, z)) => (x, y, z),
                None => (0.0, 0.0, 0.0),
            };
            // Then apply to body
            if let Some(body) = body_csg {
                Some(body.translate(Vector3::new(tx, ty, tz)))
            } else {
                Some(CSG::new().translate(Vector3::new(tx, ty, tz)))
            }
        }

        "rotate" => {
            // e.g. `rotate([rx, ry, rz]) { body }`
            let (rx, ry, rz) = match extract_vec3(&arg_values) {
                Some((x, y, z)) => (x, y, z),
                None => (0.0, 0.0, 0.0),
            };
            if let Some(body) = body_csg {
                Some(body.rotate(rx, ry, rz))
            } else {
                Some(CSG::new()) // rotating empty
            }
        }

        "scale" => {
            // e.g. `scale([sx, sy, sz]) { body }`
            let (sx, sy, sz) = match extract_vec3(&arg_values) {
                Some((x, y, z)) => (x, y, z),
                None => (1.0, 1.0, 1.0),
            };
            if let Some(body) = body_csg {
                Some(body.scale(sx, sy, sz))
            } else {
                Some(CSG::new())
            }
        }

        // If not recognized:
        _ => {
            eprintln!("Ignoring unknown module call: {}", ident_str);
            body_csg // just return the body if any
        }
    }
}

/// Parse an "arguments" node to extract numeric arguments in naive fashion.
/// This is very incomplete. Real usage must parse named args, arrays, etc.
fn parse_arguments(args_node: Node, code: &str) -> Vec<f64> {
    // The grammar: arguments => '(' [ expression or assignment ] ( ',' expression/assignment )* ')'
    // We’ll just look for numeric or bracketed array. 
    // For a real interpreter, you'd parse expressions, named parameters, etc.
    let mut vals = Vec::new();
    for child in args_node.children(&mut args_node.walk()) {
        let kind = child.kind();
        if kind == "number" {
            // Grab the text, parse as f64
            if let Some(num_str) = child.utf8_text(code.as_bytes()).ok() {
                if let Ok(val) = num_str.parse::<f64>() {
                    vals.push(val);
                }
            }
        } else if kind == "expression" || kind == "unary_expression" || kind == "binary_expression" {
            // Potentially more complicated. We can look deeper for number tokens:
            // For simplicity, we just do a DFS for any "number" child:
            let mut stack = vec![child];
            while let Some(n) = stack.pop() {
                if n.kind() == "number" {
                    if let Ok(txt) = n.utf8_text(code.as_bytes()) {
                        if let Ok(val) = txt.parse::<f64>() {
                            vals.push(val);
                        }
                    }
                } else {
                    for c in n.children(&mut n.walk()) {
                        stack.push(c);
                    }
                }
            }
        }
    }
    vals
}

/// Utility: gather child polygons if `body_csg` is itself a union of multiple shapes.
fn collect_child_csg(body: CSG) -> Vec<CSG> {
    // If we had stored the children as separate polygons, we can’t trivially break them
    // back out. So an alternative is to do a special "multi" approach from the start.
    // For demonstration, we just return [body].
    vec![body]
}

// For a simplistic approach to interpret `[x, y, z]` from the parsed arguments
fn extract_vec3(vals: &[f64]) -> Option<(f64, f64, f64)> {
    if vals.len() == 3 {
        Some((vals[0], vals[1], vals[2]))
    } else {
        None
    }
}

fn extract_cube_params(vals: &[f64]) -> Option<(f64,f64,f64,bool)> {
    // Suppose the user wrote `cube([sx,sy,sz], center=true);`
    // We’re only collecting numbers in parse_arguments, so booleans or named
    // arguments are not recognized in this naive example. Let's guess:
    if vals.len() == 3 {
        Some((vals[0], vals[1], vals[2], false))
    } else if vals.len() == 4 {
        // Maybe the 4th is 1 => center=true?
        let center = vals[3].abs() > 0.5;
        Some((vals[0], vals[1], vals[2], center))
    } else {
        None
    }
}

fn extract_square_params(vals: &[f64]) -> Option<(f64,f64,bool)> {
    if vals.len() == 2 {
        Some((vals[0], vals[1], false))
    } else if vals.len() == 3 {
        // 3rd might be center
        let center = vals[2].abs() > 0.5;
        Some((vals[0], vals[1], center))
    } else {
        None
    }
}

