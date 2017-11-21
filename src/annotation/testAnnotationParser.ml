open AnnotationParser

let _ =
  let test = "{ \"package\": \"GCD\",
  \"created\" : \"2015-07-09T08:26:42-04:00\",
  \"functions\": [
      {
          \"name\": \"mod\",
          \"args\" : [ { \"varName\" : \"a\",
                       \"varType\" : \"int\"},
                     { \"varName\" : \"b\",
                       \"varType\" : \"int\"}],
          \"secretArgs\" : [1, 2],
          \"complexity\" : { \"upperTime\" : \"a + b\",
                           \"lowerTime\" : \"Constant\",
                           \"upperMemory\" : \"Constant\",
                           \"lowerMemory\" : \"UNKNOWN\" },
          \"pure\" : true
      }
  ]
}" in
  let jsonRep = Yojson.Basic.from_string test in
  Printf.printf "Parsing\n%s\n" test;
  parseRecord jsonRep
