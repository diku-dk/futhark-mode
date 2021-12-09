-- Allow different ways to write multi-line function parameters.  There might
-- not be one good way.  The easy way is just to disable the automatic
-- indentation for this family of cases.

def foo [n] (n_steps: i32) (epsilon: f32) (time_step: f32)
             (bodies: [n]body): [n]body =
  baz

def foo
  (n_steps: i32) (epsilon: f32) (time_step: f32)
  (bodies: [n]body): [n]body =
  baz
