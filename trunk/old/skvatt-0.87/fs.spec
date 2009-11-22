
%
% Thistle diagram specification file for FS
%

diagram_class( "FS syntax trees", "0.1" )

dtd_name( "fs.dtd" )

% start -> object
diagram_spec( top, 
	var(a,object) 
)

% object -> record | recordheading | tree | list | ref | refdef | term | string
diagram_union( object, 
	[ record, 
	  recordheading,
	  tree,
	  list,
	  ref,
	  refdef,
	  term,
	  string ] 
)

% record -> bracket(vbox(row*))
diagram_spec( record, 
	bracket( [delimiter(square)],
		vbox( [align(left)],
			var(a,[row]) 
		) 
	) 
)

% recordheading -> hbox( object record )
diagram_spec( recordheading, 
	hbox( [align(top),separator(plain(" : "))],
		[ var(a,object),
		  var(b,record) ]
	) 
)

% row -> array_element( smallcaps(Text) object )
diagram_spec( row, 
	array_element( [align(center)],
		[ smallcaps(var(a,Text)), 
		  var(b,object) ]
	) 
)

% tree -> tree( object object* )
diagram_spec( tree,
	tree( [padding(space([width(20)]))],
		[ var(a,object),
		  var(b,[object]) ]
	)
)

% list -> bracket(hbox(object*))
diagram_spec( list, 
	bracket( [delimiter(angle)],
		hbox( [align(centre),separator(plain(" , "))],
			var(a,[object]) 
		)
	) 
)

% ref -> box(Text)
diagram_spec( ref,
	box( smallcaps(var(a,Text)) )
)

% refdef -> hbox( box(Text) object )
diagram_spec( refdef,
	hbox( [align(centre)],
		[ box( smallcaps(var(a,Text)) ),
		  var(b,object) ]
	)
)

% term -> plain(Text)
diagram_spec( term, 
	plain( var(a,Text) ) 
)

% string -> italic(Text)
diagram_spec( string, 
	italic( var(a,Text) ) 
)

start


