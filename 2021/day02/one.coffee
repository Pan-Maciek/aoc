stderr = process.stderr
stdout = process.stdout

unless process.argv.length == 3
  stderr.write "Usage: coffee one.coffee <input>\n"
  process.exit(1)

fs = require('fs')
readline = require('readline')

rl = readline.createInterface
  output: null
  input: fs.createReadStream(process.argv[2])
  terminal: false

Function::property = (prop, desc) ->
  Object.defineProperty @prototype, prop, desc

class State
  constructor: (options) ->
    { @distance, @depth } = options

  up: (value) -> @depth -= value
  down: (value) -> @depth += value
  forward: (value) -> @distance += value

  execute: ({command, value}) ->
    @[command](value)
  
  @property 'solution',
    get: -> @depth * @distance

class Command
  constructor: (@command, @value) ->

  @pattern = ///
    ^(?<command>up|down|forward) # command
    \s+                          # separator
    (?<value>\d+)$               # value
  ///
  @parse = (line) ->
    match = line.match @pattern
    unless match?
      stderr.write "Invalid command: #{line}\n"
      process.exit(1)
    {command, value} = match.groups
    return new Command command, parseInt value

state = new State distance: 0, depth: 0

try
  commands = (Command.parse line for await line from rl)
catch e
  stderr.write "#{e}\n"
  process.exit(1)

for command in commands
  state.execute command

stdout.write "#{state.solution}\n"