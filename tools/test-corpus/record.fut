let init (h: i32) (w: i32): state =
  {w, h,
   triangles=
     [ { p0={x=310, y=0, z=500}
       , p1={x=800, y=800, z=400}
       , p2={x=320, y=800, z=300}
       , color=0xff0000ff
       }
     , { p0={x=100, y=150, z=550}
       , p1={x=600, y=440, z=430}
       , p2={x=120, y=400, z=300}
       , color=0xffff0000
       }
     ]}
