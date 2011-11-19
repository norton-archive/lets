

#Module lets_drv#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_all_objects-1">delete_all_objects/1</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-4">destroy/4</a></td><td></td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td></td></tr><tr><td valign="top"><a href="#info_memory-1">info_memory/1</a></td><td></td></tr><tr><td valign="top"><a href="#info_size-1">info_size/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert_new-2">insert_new/2</a></td><td></td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_element-3">lookup_element/3</a></td><td></td></tr><tr><td valign="top"><a href="#match-1">match/1</a></td><td></td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td></td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td></td></tr><tr><td valign="top"><a href="#match_delete-2">match_delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#match_object-1">match_object/1</a></td><td></td></tr><tr><td valign="top"><a href="#match_object-2">match_object/2</a></td><td></td></tr><tr><td valign="top"><a href="#match_object-3">match_object/3</a></td><td></td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td></td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-4">open/4</a></td><td></td></tr><tr><td valign="top"><a href="#prev-2">prev/2</a></td><td></td></tr><tr><td valign="top"><a href="#repair-4">repair/4</a></td><td></td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td></td></tr><tr><td valign="top"><a href="#select-2">select/2</a></td><td></td></tr><tr><td valign="top"><a href="#select-3">select/3</a></td><td></td></tr><tr><td valign="top"><a href="#select_count-2">select_count/2</a></td><td></td></tr><tr><td valign="top"><a href="#select_delete-2">select_delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#select_reverse-1">select_reverse/1</a></td><td></td></tr><tr><td valign="top"><a href="#select_reverse-2">select_reverse/2</a></td><td></td></tr><tr><td valign="top"><a href="#select_reverse-3">select_reverse/3</a></td><td></td></tr><tr><td valign="top"><a href="#tab2list-1">tab2list/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="delete-1"></a>

###delete/1##




`delete(Tab) -> any()`

<a name="delete-2"></a>

###delete/2##




`delete(Tab, Key) -> any()`

<a name="delete_all_objects-1"></a>

###delete_all_objects/1##




`delete_all_objects(Tab) -> any()`

<a name="destroy-4"></a>

###destroy/4##




`destroy(Tab, Options, ReadOptions, WriteOptions) -> any()`

<a name="first-1"></a>

###first/1##




`first(Tab) -> any()`

<a name="foldl-3"></a>

###foldl/3##




`foldl(Fun, Acc0, Tab) -> any()`

<a name="foldr-3"></a>

###foldr/3##




`foldr(Fun, Acc0, Tab) -> any()`

<a name="info_memory-1"></a>

###info_memory/1##




`info_memory(Tab) -> any()`

<a name="info_size-1"></a>

###info_size/1##




`info_size(Tab) -> any()`

<a name="insert-2"></a>

###insert/2##




`insert(Tab, Object) -> any()`

<a name="insert_new-2"></a>

###insert_new/2##




`insert_new(Tab, Object) -> any()`

<a name="last-1"></a>

###last/1##




`last(Tab) -> any()`

<a name="lookup-2"></a>

###lookup/2##




`lookup(Tab, Key) -> any()`

<a name="lookup_element-3"></a>

###lookup_element/3##




`lookup_element(Tab, Key, Pos) -> any()`

<a name="match-1"></a>

###match/1##




`match(Cont) -> any()`

<a name="match-2"></a>

###match/2##




`match(Tab, Pattern) -> any()`

<a name="match-3"></a>

###match/3##




`match(Tab, Pattern, Limit) -> any()`

<a name="match_delete-2"></a>

###match_delete/2##




`match_delete(Tab, Pattern) -> any()`

<a name="match_object-1"></a>

###match_object/1##




`match_object(Cont) -> any()`

<a name="match_object-2"></a>

###match_object/2##




`match_object(Tab, Pattern) -> any()`

<a name="match_object-3"></a>

###match_object/3##




`match_object(Tab, Pattern, Limit) -> any()`

<a name="member-2"></a>

###member/2##




`member(Tab, Key) -> any()`

<a name="next-2"></a>

###next/2##




`next(Tab, Key) -> any()`

<a name="open-4"></a>

###open/4##




`open(Tab, Options, ReadOptions, WriteOptions) -> any()`

<a name="prev-2"></a>

###prev/2##




`prev(Tab, Key) -> any()`

<a name="repair-4"></a>

###repair/4##




`repair(Tab, Options, ReadOptions, WriteOptions) -> any()`

<a name="select-1"></a>

###select/1##




`select(Cont0) -> any()`

<a name="select-2"></a>

###select/2##




`select(Tab, Spec) -> any()`

<a name="select-3"></a>

###select/3##




`select(Tab, Spec, Limit) -> any()`

<a name="select_count-2"></a>

###select_count/2##




`select_count(Tab, Spec) -> any()`

<a name="select_delete-2"></a>

###select_delete/2##




`select_delete(Tab, Spec) -> any()`

<a name="select_reverse-1"></a>

###select_reverse/1##




`select_reverse(Cont0) -> any()`

<a name="select_reverse-2"></a>

###select_reverse/2##




`select_reverse(Tab, Spec) -> any()`

<a name="select_reverse-3"></a>

###select_reverse/3##




`select_reverse(Tab, Spec, Limit) -> any()`

<a name="tab2list-1"></a>

###tab2list/1##




`tab2list(Tab) -> any()`

