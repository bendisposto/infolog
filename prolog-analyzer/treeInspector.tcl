
namespace eval ::treeInspector:: {
    namespace export openTreeInspector
    namespace export resetTreeInspector
}

proc ::treeInspector::openTreeInspector {TreeType} {
    set f .treeInspectorView_$TreeType

    global tcl_version
    if {$tcl_version<8.5} {
            tkErrorBox "You have Tcl/Tk Version $tcl_version.\nThe AST inspector view requires 8.5 or newer."
    } elseif [winfo exists $f] {
        raise $f
    } else {
        if [Dialog_Create $f "Tree Inspector ($TreeType)" -borderwidth 10 -width 400] {
            set CMD "Command"
            bind $f <$CMD-w> "destroy $f"
            #prolog "use_module(probsrc(tcltk_tree_inspector))"
            fillTreeInspector $f $TreeType
        }
    }
}

proc ::treeInspector::resetTreeInspector {TreeType} {
    set f .treeInspectorView_$TreeType
    # prolog reset_tcltk_tree_inspector
    if [winfo exists $f] {
        # fillTreeInspector $f empty
        # to do: update rather than destroy view
        destroy $f
    }
}

proc ::treeInspector::fillTreeInspector {f TreeType} {
    foreach {c} [winfo children $f] {
        destroy $c
    }

    set t $f.tree

    scrollbar $f.scrolly -orient vertical   -command "$t yview"
    scrollbar $f.scrollx -orient horizontal -command "$t xview"
    global tti_col
    prolog ttii_number_of_columns($TreeType,NrCols)
    # puts "Tree $TreeType $prolog_variables(NrCols)"
    
    set VisibleCols {}
    for {set si 0} {$si < $prolog_variables(NrCols)} {incr si} {
	    lappend VisibleCols tti_col($si)
	}
	
    #set VisibleCols {tti_col(0) tti_col(1) tti_col(2) tti_col(3)}
	set AllCols $VisibleCols
	lappend AllCols {index}
	# puts "Columns = '$VisibleCols' (all = '$AllCols')"
    
    ttk::treeview $t -columns $AllCols -displaycolumns $VisibleCols -yscrollcommand "$f.scrolly set" -xscrollcommand "$f.scrollx set"
    
    prolog ttii_column_info($TreeType,header,Name,MinWidth,Width,Anchor)
    
    $t column #0 -minwidth $prolog_variables(MinWidth) -width "$prolog_variables(Width)"
    $t column #0 -anchor "$prolog_variables(Anchor)"
    $t heading #0 -text "$prolog_variables(Name)"
    $t tag configure inac   -foreground grey
    $t tag configure error  -foreground red
    $t tag configure ptrue  -foreground darkgreen
    $t tag configure pfalse -foreground darkred
    $t tag configure darkblue -foreground darkblue
    $t tag configure darkgray -foreground darkgray
    $t tag configure gray5 -foreground gray5
    $t tag configure unseen -foreground yellow -background black
    $t tag configure marked -foreground gray30
    $t tag configure marked -background snow2
    $t tag configure testcase -foreground black
    # $t tag configure testcase -background honeydew
    
    for {set si 0} {$si < $prolog_variables(NrCols)} {incr si} {
		if [prolog ttii_column_info($TreeType,$si,Name,MinWidth,Width,Anchor)] {
			# puts "got column $si $prolog_variables(Name) $prolog_variables(Anchor)"
			$t column tti_col($si)  -minwidth $prolog_variables(MinWidth)
			$t column tti_col($si)  -width $prolog_variables(Width)
			$t column tti_col($si)  -anchor $prolog_variables(Anchor)
			$t heading tti_col($si)  -text $prolog_variables(Name)
		} else {
			$t column tti_col($si)  -minwidth 10
			$t heading tti_col($si)  -text "???"
		}
	}
    

    # The index of the root element is []
    $t set {} index ""
    initItem $t {} $TreeType
    initChildren $t {} $TreeType

    bind $t <<TreeviewOpen>> [namespace code "openTREEnode $t $TreeType"]

    pack $f.scrolly -side right -fill y
    pack $t -fill both -expand yes
    pack $f.scrollx -side bottom -fill x
}

proc ::treeInspector::openTREEnode {t TreeType} {
    initChildren $t [$t focus] $TreeType
}

proc ::treeInspector::initChildren {t elem TreeType} {
    foreach child [$t children $elem] {
        initItem $t $child $TreeType
    }
}

proc ::treeInspector::initItem {t elem TreeType} {
    if [$t tag has opened $elem] {
        return
    }
    # The index has a form x,y,z
    set index [$t set $elem index]
    # As Prolog list it is [x,y,z]
    set pindex "\[$index\]"
    # Prepare to add an item: Add a comma if the list is not empty
    if {$index == ""} {
        set cindex $index
    } {
        set cindex "$index,"
    }
    if [prolog ttii_get_node_info($TreeType,$pindex,Txt,Columns,NrSubs,Tags)] {
        $t item $elem -text "$prolog_variables(Txt)"
        set cols $prolog_variables(Columns)
        
		  set nrcols [llength $prolog_variables(Columns)]
		    puts "nr cols $nrcols for $prolog_variables(Columns), tags = $prolog_variables(Tags)"
		  for {set i 0} {$i < $nrcols} {incr i 1} {
			set ColContent [lindex $prolog_variables(Columns) $i]
			 puts "col $i : $ColContent"
			$t set $elem tti_col($i) $ColContent
		  }
	    $t item $elem -tags [list $prolog_variables(Tags)]
        set n $prolog_variables(NrSubs)
        for {set si 0} {$si < $n} {incr si} {
            set f [$t insert $elem end]
            $t set $f index "$cindex$si"
        }
        $t tag add opened $elem
    }
}


proc Dialog_Create {top title args} {
	global dialog tcl_version
	if [winfo exists $top] {
		switch -- [wm state $top] {
			normal {
				# Raise a buried window
				raise $top
			}
			withdrawn -
			iconified {
				# Open and restore geometry
				wm deiconify $top
				catch {wm geometry $top $dialog(geo,$top)}
			}
		}
		return 0
	} else {
		eval {toplevel $top} $args
		wm title $top $title
        if {"darwin" == "darwin"} {
            # Hiding the main menu bar
            menu $top.m -tearoff 0
            $top config -menu $top.m
        }
		return 1
	}
}
proc Dialog_WaitVisible {top {focus {}}} {
	# Grab focus for the dialog
	if {[string length $focus] == 0} {
		set focus $top
	}
	set old [focus -displayof $top]
	focus $focus
	catch {tkwait visibility $top}
	catch {grab $top}
}
proc Dialog_Wait {top varName {focus {}}} {
	upvar $varName var

	# Poke the variable if the user nukes the window
	bind $top <Destroy> [list set $varName $var]

	# Grab focus for the dialog
	if {[string length $focus] == 0} {
		set focus $top
	}
	set old [focus -displayof $top]
	focus $focus
	catch {tkwait visibility $top}
	catch {grab $top}

	# Wait for the dialog to complete
	tkwait variable $varName
	catch {grab release $top}
	focus $old
}

proc Dialog_Dismiss {top} {
	global dialog
	# Save current size and position
	catch {
		# window may have been deleted
		set dialog(geo,$top) [wm geometry $top]
		wm withdraw $top
	}
}

