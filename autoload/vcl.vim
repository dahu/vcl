" Vim Control Language (a tiny TCL-ish for Vim)
" Barry Arthur, April 2015
" Ported from http://antirez.com/picol/picol.c.txt

let vcl#state = { 'OK' : 'OK', 'ERR' : 'ERR', 'RETURN' : 'RETURN', 'BREAK' : 'BREAK', 'CONTINUE' : 'CONTINUE' }
let vcl#token = { 'ESC' : 'ESC', 'STR' : 'STR', 'CMD' : 'CMD', 'VAR' : 'VAR', 'SEP' : 'SEP', 'EOL' : 'EOL', 'EOF' : 'EOF' }

"
" Parser
"

function! vcl#Parser(text)
  let p = {}
  let p.text     = a:text
  let p.len      = len(a:text)
  let p.start    = 0
  let p.pos      = 0
  let p.end      = 0
  let p.in_quote = 0
  let p.type     = g:vcl#token.EOL

  func p.char()
    return self.text[self.pos]
  endfunc

  func p.match(pat)
    return self.char() =~ a:pat
  endfunc

  func p.skip_char()
    let self.pos += 1
    let self.len -= 1
  endfunc

  func p.skip(pat)
    while self.match(a:pat)
      if self.len < 0
        throw 'VCL Parser (ERROR): Unexpected end of input.'
      endif
      call self.skip_char()
    endwhile
  endfunc

  func p.mark_start()
    let self.start = self.pos
  endfunc

  func p.mark_end()
    let self.end = self.pos - 1
  endfunc

  func p.parse_separator()
    call self.mark_start()
    call self.skip('[ \t\n\r]')
    call self.mark_end()
    let self.type = g:vcl#token.SEP
    return g:vcl#state['OK']
  endfunc

  func p.parse_eol()
    call self.mark_start()
    call self.skip('[ \t\n\r;]')
    call self.mark_end()
    let self.type = g:vcl#token.EOL
    return g:vcl#state['OK']
  endfunc

  func p.parse_command()
    let level = 1
    let blevel = 0
    let self.pos += 1
    call self.mark_start()
    let self.len -= 1
    while 1
      if self.len == 0
        break
      elseif self.char() == '[' && blevel == 0
        let level += 1
      elseif self.char() == ']' && blevel == 0
        let level -= 1
        if level == 0
          break
        endif
      elseif self.char() == '\\'
        call self.skip_char()
      elseif self.char() == '{'
        let blevel += 1
      elseif self.char() == '}'
        if blevel != 0
          let blevel -= 1
        endif
      endif
      call self.skip_char()
    endwhile
    call self.mark_end()
    let self.type = g:vcl#token.CMD
    if self.char() == ']'
      call self.skip_char()
    endif
    return g:vcl#state['OK']
  endfunc

  func p.parse_variable()
    let self.pos += 1
    call self.mark_start()
    let self.len -= 1     " skip the $
    call self.skip('\w')
    call self.mark_end()
    if self.start == self.pos     " It's just a single char string '$'
      let self.start = self.end
      let self.type = g:vcl#token.STR
    else
      let self.type = g:vcl#token.VAR
    endif
    return g:vcl#state.OK
  endfunc

  func p.parse_brace()
    let level = 1
    let self.pos += 1
    call self.mark_start()
    let self.len -= 1
    while 1
      let ch = self.char()
      if self.len >= 2 && ch == '\\'
        call self.skip_char()
      elseif self.len == 0 || ch == '}'
        let level -= 1
        if level == 0 || self.len == 0
          call self.mark_end()
          if self.len != 0
            call self.skip_char()
          endif
          let self.type = g:vcl#token.STR
          return g:vcl#state.OK
        endif
      elseif ch == '{'
        let level += 1
      endif
      call self.skip_char()
    endwhile
  endfunc

  func p.parse_string()
    let newword = index([g:vcl#token.SEP, g:vcl#token.EOL, g:vcl#token.STR], self.type) != -1
    if newword && self.char() == '{'
      return self.parse_brace()
    elseif newword && self.char() == '"'
      let self.in_quote = 1
      call self.skip_char()
    endif
    call self.mark_start()
    while 1
      if self.len == 0
        call self.mark_end()
        let self.type = g:vcl#token.ESC
        return g:vcl#state.OK
      endif
      let ch = self.char()
      if ch == '\'
        if self.len >= 2
          call self.skip_char()
        endif
      elseif ch == '$' || ch == '['
        call self.mark_end()
        let self.type = g:vcl#token.ESC
        return g:vcl#state.OK
      elseif ch =~ '[ \t\n\r;]'
        if ! self.in_quote
          call self.mark_end()
          let self.type = g:vcl#token.ESC
          return g:vcl#state.OK
        endif
      elseif ch == '"'
        if self.in_quote
          call self.mark_end()
          let self.type = g:vcl#token.ESC
          call self.skip_char()
          let self.in_quote = 0
          return g:vcl#state.OK
        endif
      endif
      call self.skip_char()
    endwhile
  endfunc

  func p.parse_comment()
    while self.char() != "\n"
      call self.skip_char()
    endwhile
    return g:vcl#state.OK
  endfunc

  func p.get_next_token()
    while 1
      if self.len == 0
        if index([g:vcl#token.EOL, g:vcl#token.EOF], self.type) == -1
          let self.type = g:vcl#token.EOL
        else
          let self.type = g:vcl#token.EOF
        endif
        return g:vcl#state.OK
      endif
      let ch = self.char()
      if ch =~ '[ \t\r]'
        if self.in_quote
          return self.parse_string()
        endif
        return self.parse_separator()
      elseif ch =~ '[;\n]'
        if self.in_quote
          return self.parse_string()
        endif
        return self.parse_eol()
      elseif ch == '['
        return self.parse_command()
      elseif ch == '$'
        return self.parse_variable()
      elseif ch == '#'
        if self.type == g:vcl#token.EOL
          call self.parse_comment()
          continue
        endif
        return self.parse_string()
      else
        return self.parse_string()
      endif
    endwhile
  endfunc

  func p.token()
    return strpart(self.text, self.start, (self.end - self.start + 1))
  endfunc

  return p
endfunc

"
" Interpreter
"

function! vcl#Interpreter()
  let i = {}
  let i.level = 0
  let i.call_frame = {}
  let i.call_frame.vars = {}
  let i.call_frame.parent = {}
  let i.commands = {}
  let i.result = ''

  func i.get_var(name)
    return get(self.call_frame.vars, a:name, {})
  endfunc

  func i.set_var(name, val)
    let v = self.get_var(a:name)
    if ! empty(v)
      let v.val = a:val
    else
      let v.name = a:name
      let v.val = a:val
      call extend(self.call_frame.vars, {a:name : v})
    endif
    return g:vcl#state.OK
  endfunc

  func i.get_command(name)
    return get(self.commands, a:name, {})
  endfunc

  func i.set_command(name, func, priv_data)
    let c = self.get_command(a:name)
    if ! empty(c)
      throw 'VCL Interp (ERROR): ' . a:name . ' already defined.'
    endif
    let c.name = a:name
    let c.func = a:func
    let c.priv_data = a:priv_data
    call extend(self.commands, {a:name : c})
    return g:vcl#state.OK
  endfunc


  func i.eval(text)
    let p = vcl#Parser(a:text)
    let args = []
    let self.result = ''
    while 1
      let prev_type = p.type
      call p.get_next_token()
      if p.type == g:vcl#token.EOF
        break
      endif
      let t = p.token()
      if p.type == g:vcl#token.VAR
        let v = self.get_var(t)
        if empty(v)
          throw 'VCL Interp (ERROR): No such variable ' . t
          return g:vcl#state.ERR
        endif
        let t = v.val
      elseif p.type == g:vcl#token.CMD
        " try
        let retcode = self.eval(t)
        " catch
        if retcode != g:vcl#state.OK
          return retcode
        endif
        " endtry
        let t = self.result
      elseif p.type == g:vcl#token.ESC
        " TODO: escape handling missing!
      elseif p.type == g:vcl#token.SEP
        let prev_type = p.type
        continue
      endif
      " We have a complete command + args. Call it!
      if p.type == g:vcl#token.EOL
        let prev_type = p.type
        if ! empty(args)
          let c = self.get_command(args[0])
          if empty(c)
            throw 'VCL Interp (ERROR): No such command ' . args[0]
            return g:vcl#state.ERR
          endif
          let retcode = call(c.func, [args, c.priv_data], self)
          if retcode != g:vcl#state.OK
            return g:vcl#state.ERR
          endif
          " Prepare for the next command
          let args = []
          continue
        endif
      endif

      " We have a new token, append to the previous or as new arg?
      if prev_type == g:vcl#token.SEP || prev_type == g:vcl#token.EOL
        call add(args, t)
      else     " Interpolation
        let args[-1] = t
      endif
      let prev_type = p.type
    endwhile
  endfunc

  """ Commands

  func i.command_math(args, priv_data)
    let args = a:args
    if len(args) != 3
      throw 'VCL Interp (ERROR): Wrong number of args for ' . args[0]
    endif
    let a = str2nr(args[1])
    let b = str2nr(args[2])
    let op = args[0]
    if op == '+'
      let c = a + b
    elseif op == '-'
      let c = a - b
    elseif op == '*'
      let c = a * b
    elseif op == '/'
      let c = a / b
    elseif op == '%'
      let c = a % b
    elseif op == '>'
      let c = a > b
    elseif op == '>='
      let c = a >= b
    elseif op == '<'
      let c = a < b
    elseif op == '<='
      let c = a <= b
    elseif op == '=='
      let c = a == b
    elseif op == '!='
      let c = a != b
    else
      let c = 0
    endif
    let self.result = c
    return g:vcl#state.OK
  endfunc

  func i.register_core_commands()
    for op in [ '+', '-', '*', '/', '%', '>', '>=', '<', '<=', '==', '!=' ]
        call self.set_command(op, self.command_math, {})
    endfor
    " call self.set_command("set"      , self.command_set       , {})
    " call self.set_command("puts"     , self.command_puts      , {})
    " call self.set_command("if"       , self.command_if        , {})
    " call self.set_command("while"    , self.command_while     , {})
    " call self.set_command("break"    , self.command_ret_codes , {})
    " call self.set_command("continue" , self.command_ret_codes , {})
    " call self.set_command("proc"     , self.command_proc      , {})
    " call self.set_command("return"   , self.command_return    , {})
  endfunc

  call i.register_core_commands()
  return i
endfunc

"
" Test
"

if expand('%:p') == expand('<sfile>:p')

  "
  " Parser
  "

  let p = vcl#Parser("   \t\t\t\n\n\n\r\r\r")
  echo [p.parse_separator(), p.len, p.pos, 12, p.end, 11]

  let p = vcl#Parser(";\n")
  echo [p.parse_eol(), p.len, p.pos, 2, p.end, 1]

  let p = vcl#Parser('[set x]')
  echo [p.parse_command(), p.len, p.pos, 7, p.end, 5]

  let p = vcl#Parser('$')
  echo [p.parse_variable(), p.len, p.pos, 1, p.end, 0]

  let p = vcl#Parser('$foo')
  echo [p.parse_variable(), p.len, p.pos, 4, p.end, 3]

  let p = vcl#Parser('{test brace}')
  echo [p.parse_brace(), p.len, p.pos, 12, p.end, 10]

  let p = vcl#Parser('"test string"')
  echo [p.parse_string(), p.len, p.pos, 13, p.end, 11]

  let p = vcl#Parser("test comment\n")
  echo [p.parse_comment(), p.len, p.pos, 12, p.end, 0]
  echo [p.parse_eol(), p.len, p.pos, 13, p.end, 12]

  let prog = 'proc square {x} { * $x $x }'
  echo prog
  let p = vcl#Parser(prog)
  call p.get_next_token()
  while p.type != vcl#token.EOF
    echo [p.type, p.token()]
    call p.get_next_token()
  endwhile

  "
  " Interpreter
  "

  let i = vcl#Interpreter()

  echo i.set_var('foo', 10)
  echo i.get_var('foo')

  function! vcl#foo(args, data)
    echom "foo!"
  endfunction

  echo i.set_command('foo', 'vcl#foo', {})
  echo i.get_command('foo')

  let prog = '* 6 [+ 5 2]'
  call i.eval(prog)
  echo i.result

endif

