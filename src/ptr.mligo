type ptr = nat

[@inline] let ptr_null = 0n
[@inline] let ptr_next (t: ptr) = t + 1n
