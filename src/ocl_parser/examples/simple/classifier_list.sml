val it =
  [Class
     {activity_graphs=[],associationends=[],
      attributes=[{attr_type=Integer,init=NONE,name="r",scope=InstanceScope,
                   stereotypes=[],visibility=public}],interfaces=[],
      invariant=[],name=["simple","E"],operations=[],
      parent=SOME ["simple","B"],stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[{attr_type=Real,init=NONE,name="r",scope=InstanceScope,
                   stereotypes=[],visibility=public}],interfaces=[],
      invariant=[],name=["simple","D"],operations=[],
      parent=SOME ["simple","A"],stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[{attr_type=String,init=NONE,name="s",scope=InstanceScope,
                   stereotypes=[],visibility=public}],interfaces=[],
      invariant=[],name=["simple","C"],operations=[],
      parent=SOME ["simple","A"],stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[{attr_type=Set (Classifier ["simple","A"]),init=NONE,
                   name="a",scope=InstanceScope,stereotypes=[],
                   visibility=public},
                  {attr_type=Integer,init=NONE,name="j",scope=InstanceScope,
                   stereotypes=[],visibility=public},
                  {attr_type=Classifier ["simple","A"],init=NONE,
                   name="attribA",scope=InstanceScope,stereotypes=[],
                   visibility=public}],interfaces=[],
      invariant=[(SOME "multconstraint_for_aend_a",
                  OperationCall
                    (OperationCall
                       (OperationCall
                          (AttributeCall
                             (Variable ("self",Classifier ["simple","B"]),
                              Classifier ["simple","B"],["simple","B","a"],
                              Set (Classifier ["simple","A"])),
                           Set (Classifier ["simple","A"]),
                           ["oclLib","Collection","size"],[],Integer),Integer,
                        ["oclLib","Real",">="],
                        [(Literal ("1",Integer),Integer)],Boolean),Boolean,
                     ["oclLib","Boolean","and"],
                     [(OperationCall
                         (OperationCall
                            (AttributeCall
                               (Variable ("self",Classifier ["simple","B"]),
                                Classifier ["simple","B"],["simple","B","a"],
                                Set (Classifier ["simple","A"])),
                             Set (Classifier ["simple","A"]),
                             ["oclLib","Collection","size"],[],Integer),
                          Integer,["oclLib","Real","<="],
                          [(Literal ("5",Integer),Integer)],Boolean),Boolean)],
                     Boolean))],name=["simple","B"],operations=[],parent=NONE,
      stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[{attr_type=Classifier ["simple","B"],init=NONE,name="b",
                   scope=InstanceScope,stereotypes=[],visibility=public},
                  {attr_type=Integer,init=NONE,name="i",scope=InstanceScope,
                   stereotypes=[],visibility=public},
                  {attr_type=Real,init=NONE,name="r",scope=InstanceScope,
                   stereotypes=[],visibility=public},
                  {attr_type=Classifier ["simple","B"],init=NONE,
                   name="attribB",scope=InstanceScope,stereotypes=[],
                   visibility=public}],interfaces=[],
      invariant=[(SOME "multconstraint_for_aend_b",
                  OperationCall
                    (AttributeCall
                       (Variable ("self",Classifier ["simple","A"]),
                        Classifier ["simple","A"],["simple","A","b"],
                        Classifier ["simple","B"]),Classifier ["simple","B"],
                     ["oclIsDefined"],[],Boolean))],name=["simple","A"],
      operations=[{arguments=[("p",Integer)],isQuery=true,name="m",
                   postcondition=[],precondition=[],result=Integer,
                   scope=InstanceScope,visibility=public}],parent=NONE,
      stereotypes=[],thyname=NONE}] : Classifier list
