PDKLib='()
LAYERMap = ""
DefaultLib='("basic", "analogLib", "ahdlLib")
ParamList='("model", "lr", "wr", "totalW", "nr", "m")

/*
   procedure(printParams(inst)
  print(inst~>?)
  print(inst~>??)
  let((cdf)
    cdf=cdfGetInstCDF(inst)
    foreach( param cdf->parameters
        unless((param~>value == param~>defValue)
        printf("%s: %L %L %L %L\n" param~>name type(param~>value) param~>valueType param~>value param~>defValue)
        )
    ) ;foreach
  ) ;let
)
*/

; TODO : replace setParam with parameter argument list to the the instance creator
procedure(setParam(inst paramName paramType value)
  unless(dbFindProp(inst paramName)
    dbReplaceProp(inst paramName paramType value)
  )
)

procedure(createLayout(lib cell inst pdklib pdkcell outdir)
  if(member(pdklib PDKLib) then
    mv=dbOpenCellViewByType(pdklib pdkcell "layout" "maskLayout" "r")
    if(mv then
      lname = strcat("__align__" inst~>name) 
      lv=dbOpenCellViewByType(lib cell strcat("layout" lname) "maskLayout" "w")
      layoutInst=dbCreateParamInst(lv mv inst~>name list(0 0) "R0" 1)
      let((cdf)
        cdf=cdfGetInstCDF(inst)
        foreach( param cdf->parameters
            unless((param~>value == param~>defValue)
              setParam(layoutInst param~>name type(param~>value) param~>value)
            )
        ) ;foreach
      ) ;let
      dbSave(lv)
      dbClose(lv)
      ; stream the layout in GDS format
      xstSetField("library" lib)
      xstSetField("topCell" cell)
      xstSetField("view" strcat("layout" lname))
      fileName = strcat(cell lname)
      xstSetField("strmFile" strcat(outdir "/" upperCase(fileName) ".gds"))
      xstSetField("showCompletionMsgBox" "false")
      xstSetField("layerMap" LAYERMap)
      xstOutDoTranslate()
    ) ;if
    dbClose(mv)
  ) ;if
) ;procedure

procedure(writeLeaf(cell ports)
  printf("writing leaf : %s %L\n" cell ports)
  fprintf(sphandle ".subckt %s" cell)
  foreach(i ports fprintf(sphandle " %s" i))
  fprintf(sphandle "\n* @: Generator(name='black_box')\n")
  fprintf(sphandle ".ends %s\n" cell)
)

procedure(getLeaf(lib cell level leaf)
  cv = dbOpenCellViewByType(lib cell "schematic")
  unless(member(lib append(DefaultLib PDKLib))
    foreach(inst cv~>instances
      unless(member(inst~>libName DefaultLib)
        sortedTerms=sort(inst~>instTerms~>name 'alphalessp)
        termNameMap = makeTable("termName" "")
        if(sortedTerms == '("B" "D" "G" "S") then sortedTerms = '("D" "G" "S" "B"))
        ;printf("%L %L %L\n" inst~>libName inst~>cellName level[list(inst~>libName inst~>cellName)])
        instName = inst~>cellName
        if(level[list(inst~>libName inst~>cellName)] == 0 then
          instName = strcat(cell "__align__" inst~>name)
          leaf[instName] = sortedTerms
        )
      ) ;foreach
    )
  )
)

procedure(writeHier(lib cell level sphandle outdir)
  printf("writing cell : %s %s " lib cell)
  cv = dbOpenCellViewByType(lib cell "schematic")
  fprintf(sphandle ".subckt %s" cell)
  sortedTerms=sort(cv~>terminals~>name 'alphalessp)
  printf("%L\n" sortedTerms)
  ; SPICE follows BDGS order for MOS devices
  if(sortedTerms == '("B" "D" "G" "S") then sortedTerms = '("D" "G" "S" "B"))
  foreach(i sort(sortedTerms 'alphalessp) fprintf(sphandle " %s" i))
  fprintf(sphandle "\n")
  unless(member(lib append(DefaultLib PDKLib))
    foreach(inst cv~>instances
      unless(member(inst~>libName DefaultLib)
        sortedTerms=sort(inst~>instTerms~>name 'alphalessp)
        termNameMap = makeTable("termName" "")
        if(sortedTerms == '("B" "D" "G" "S") then sortedTerms = '("D" "G" "S" "B"))
        ;printf("%L %L %L\n" inst~>libName inst~>cellName level[list(inst~>libName inst~>cellName)])
        masterName = inst~>cellName
        if(level[list(inst~>libName inst~>cellName)] == 0 then
          masterName = strcat(cell "__align__" inst~>name)
        )
        foreach(i inst~>instTerms termNameMap[i~>name] = i~>net~>name)
        instName = inst~>name
        if(rexMatchp("^[M]" instName) then
          instName = strcat("X" instName)
        )
        fprintf(sphandle "%s" instName)
        foreach(i sortedTerms
          if(termNameMap[i] == "gnda!" then fprintf(sphandle " gnda")
          else fprintf(sphandle " %s" termNameMap[i]))
        )
        fprintf(sphandle " %s\n" masterName)
        createLayout(lib cell inst inst~>libName inst~>cellName outdir)
      ) ;foreach
    )
  )
  fprintf(sphandle ".ends %s\n" cell)
)

procedure(_genHierLayout(lib cell level)
  let((index currLevel)
    index = list(lib cell)
    currLevel = -1
    if(level[index] < 0 then
      if(member(lib append(DefaultLib PDKLib)) then
        currLevel = 0
      else
        cv = dbOpenCellViewByType(lib cell "schematic")
        if(cv then
          foreach(inst cv~>instances
            unless(rexMatchp("^[io]+pin$" inst~>cellName)
              c = _genHierLayout(inst~>libName inst~>cellName level)
              currLevel = max(currLevel c)
            ) ;unless
          ) ;foreach
          currLevel += 1
        ) ;if 
      ) ;if
      level[index] = currLevel
    else
      currLevel = level[index]
    ) ;if
  currLevel
  ) ;let
) ;procedure

procedure(generateALIGNDeps(lib cell outdir pdklib layerMap)
  PDKLib = pdklib
  LAYERMap = layerMap
  createDir(outdir)
  level = makeTable("level" -1)
  leaf = makeTable("leaf" '())
  sphandle = outfile( strcat(outdir "/" cell ".sp") )
  _genHierLayout(lib cell level)
  let((k)
    k=list()
    foreach(i sort(tableToList(level) lambda((a b) cadr(a)<cadr(b))) if(nth(1 i) > 0 k=cons(nth(0 i) k)))
    k = reverse(k)
    foreach(i k getLeaf(nth(0 i) nth(1 i) level leaf))
    foreach(l leaf fprintf(sphandle "\n") writeLeaf(l leaf[l]))
    foreach(i k fprintf(sphandle "\n") writeHier(nth(0 i) nth(1 i) level sphandle outdir))
  )
  close(sphandle)
  t
)

