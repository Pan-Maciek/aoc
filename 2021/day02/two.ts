import { readLines } from "https://deno.land/std/io/mod.ts";

const encoder = new TextEncoder();
if (Deno.args.length !== 1) {
  Deno.stderr.write(encoder.encode("Usage: deno two.ts <input>\n"));
  Deno.exit(1);
}

class State {
  private distance: number;
  private depth: number;
  private aim: number;

  constructor();
  constructor(distance?: number, depth?: number, aim?: number) {
    this.distance = distance ?? 0;
    this.depth = depth ?? 0;
    this.aim = aim ?? 0;
  }

  up(value: number) {
    this.aim -= value;
  }

  down(value: number) {
    this.aim += value;
  }

  forward(value: number) {
    this.distance += value;
    this.depth += this.aim * value;
  }

  execute(command: Command) {
    this[command.command](command.value);
  }

  get solution() {
    return this.distance * this.depth;
  }
}

type Direction = "up" | "down" | "forward";
class Command {
  constructor(
    public readonly command: Direction,
    public readonly value: number
  ) { }

  static pattern = /^(?<command>up|down|forward)\s+(?<value>\d+)$/
  static parse(line: string): Command {
    const match = line.match(Command.pattern);
    if (match == null) {
      Deno.stderr.write(encoder.encode(`Invalid command: ${line}\n`));
      Deno.exit(1);
    }
    return new Command(
      match!.groups!.command as Direction,
      parseInt(match!.groups!.value)
    );
  }
}

const fileReader = await Deno.open(Deno.args[0]).catch((e) => {
  Deno.stderr.write(encoder.encode(`Error: ${e.message}\n`));
  Deno.exit(1);
})

const state = new State();
for await (let line of readLines(fileReader)) {
  const command = Command.parse(line);
  state.execute(command);
}

Deno.stdout.write(encoder.encode(`${state.solution}\n`));