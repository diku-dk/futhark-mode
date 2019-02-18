module mk_complex(T: real): (complex with real = T.t
                                     with complex = (T.t, T.t)) = {
  let step td (s: state) =
    let spheres' = foo s.spheres
    let lights' = bar s.lights
    in s with eye = eye'
         with screen_view_dest = screen_view_dest'
         with spheres = spheres'
         with lights = lights'

}
