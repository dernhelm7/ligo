type foobar is map(int, int)

function size_ (const m : foobar) : nat is
  block {skip} with (size(m))

function gf (const m : foobar) : int is begin skip end with get_force(23, m)

const fb : foobar = map
  23 -> 0 ;
  42 -> 0 ;
end

function get (const m : foobar) : option(int) is
  begin
    skip
  end with m[42]

const bm : foobar = map
  144 -> 23 ;
  51 -> 23 ;
  42 -> 23 ;
  120 -> 23 ;
  421 -> 23 ;
end

