#logic
a b <= ? a b < : § ? a b = : § ? $
a ~ ? a : $ ? §
> : <= ~
a b >= ? a b > : § ? a b = : § ? $
!= : = ~
a b | ? a : § ? b : § ? $
a b & : a ~ b ~ | ~

#arithmetic
a is_even : a 2 % 0 > ~
a b is_div : b a % 0 > ~
a dup : a a
a b first : a
a b second : b
a b min ? a b < : a ? b
a b max ? a b > : a ? b
base up exp  ? up 1 = : base ? base base up 1 - exp *
a ! ? a 1 = : 1 ? a a 1 - ! *
a abs ? a 0 > : a ? 0 a -

#stacks
a flatten ? a ° : ? a ; flatten a v
a f e foldl ? a ° : e ? a ; f e a v f flatten foldl
a f map ? a ° : ( ) ? a ; f map a v f flatten ^
a f filter ? a ° : ( ) ? a v f flatten : a ; f filter a v ^ ? a ; f filter
a b eq_stack ? a size b size != : 0 ? a ° : 1 ? a v b v = a ; b ; eq_stack &
a n elem_at ? a ° : ? n 1 = : a v ? a ; n 1 - elem
a size ? a ° : 0 ? 1 a ; size +
a last ? a size 1 = : a v ? a ; last
i n range ? i n = : ( ) ? i 1 + n range i ^
a b i elem_back ? a ° : -1 ? a v b = : i ? a ; b i 1 + elem_back
elem : 0 elem_back
a b reverse_back ? a ° : b ? a ; b a v ^ reverse_back
reverse : ( ) reverse_back
a b zip ? a ° b ° | : ( ) ? a ; b ; zip ( ) b v ^ a v ^ ^
a s e slice ? s 0 > : a ; s 1 - e 1 - slice ? e 0 <= : ( ) ? a ; s e 1 - slice a v ^
sum : ( + ) 0 foldl
a min_stack : a ; ( min ) a v foldl
a max_stack : a ; ( max ) a v foldl
a b concat_back ? b ° : a ? a b v ^ b ; concat_back
b concat : b reverse concat_back
