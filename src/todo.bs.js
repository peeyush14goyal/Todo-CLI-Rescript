// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Os = require("os");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");

var getToday = (function() {
  let date = new Date();
  return new Date(date.getTime() - (date.getTimezoneOffset() * 60000))
    .toISOString()
    .split("T")[0];
});

var encoding = "utf8";

var pending_todos_file = "todo.txt";

var completed_todos_file = "done.txt";

if (!Fs.existsSync(pending_todos_file)) {
  Fs.writeFileSync(pending_todos_file, "", {
        encoding: encoding,
        flag: "w"
      });
}

if (!Fs.existsSync(completed_todos_file)) {
  Fs.writeFileSync(completed_todos_file, "", {
        encoding: encoding,
        flag: "w"
      });
}

function read_file(filename) {
  var data = Fs.readFileSync("./" + filename, {
        encoding: "utf8",
        flag: "r"
      });
  var data_array = data.split("\n");
  return data_array.filter(function (x) {
              return x !== "";
            });
}

function append_file(filename, data) {
  Fs.appendFileSync("./" + filename, data, {
        encoding: "utf8",
        flag: "a"
      });
  
}

function write_file(filename, data) {
  Fs.writeFileSync("./" + filename, data, {
        encoding: encoding,
        flag: "w"
      });
  
}

function list_of_todos(param) {
  var data_array = read_file(pending_todos_file);
  if (data_array.length > 0) {
    var array = data_array.reverse();
    array.forEach(function (todo, index) {
          var index_val = String(array.length - index | 0);
          console.log("[" + index_val + "] " + todo);
          
        });
    return ;
  }
  console.log("There are no pending todos!");
  
}

function add_todos(todo) {
  append_file(pending_todos_file, todo + Os.EOL);
  console.log("Added todo: \"" + todo + "\"");
  
}

var usage = "Usage :-\n$ ./todo add \"todo item\"  # Add a new todo\n$ ./todo ls               # Show remaining todos\n$ ./todo del NUMBER       # Delete a todo\n$ ./todo done NUMBER      # Complete a todo\n$ ./todo help             # Show usage\n$ ./todo report           # Statistics";

function help(param) {
  console.log(usage);
  
}

function completed(index_val) {
  var data_array = read_file(pending_todos_file);
  if (index_val > data_array.length || index_val < 1) {
    console.log("Error: todo #" + String(index_val) + " does not exist.");
    return ;
  }
  var todo = Caml_array.get(data_array, index_val - 1 | 0);
  append_file(completed_todos_file, Curry._1(getToday, undefined) + " " + todo + Os.EOL);
  var data_array$1 = data_array.filter(function (param, index) {
        return (index + 1 | 0) !== index_val;
      });
  var data_string = data_array$1.join(Os.EOL);
  write_file(pending_todos_file, data_string);
  console.log("Marked todo #" + String(index_val) + " as done.");
  
}

function delete_todo(index_val) {
  var data_array = read_file(pending_todos_file);
  if (index_val > data_array.length || index_val < 1) {
    console.log("Error: todo #" + String(index_val) + " does not exist. Nothing deleted.");
    return ;
  }
  var data_array$1 = data_array.filter(function (param, index) {
        return (index + 1 | 0) !== index_val;
      });
  var data_string = data_array$1.join(Os.EOL);
  write_file(pending_todos_file, data_string);
  console.log("Deleted todo #" + String(index_val));
  
}

function report(param) {
  var pending = read_file(pending_todos_file);
  var completed = read_file(completed_todos_file);
  var pending_count = String(pending.length);
  var completed_count = String(completed.length);
  console.log(Curry._1(getToday, undefined) + " Pending : " + pending_count + " Completed : " + completed_count);
  
}

function exe_command(command) {
  if (typeof command === "number") {
    switch (command) {
      case /* Help */0 :
          console.log(usage);
          return ;
      case /* Report */1 :
          return report(undefined);
      case /* List */2 :
          return list_of_todos(undefined);
      case /* Empty */3 :
          return ;
      
    }
  } else {
    switch (command.TAG | 0) {
      case /* Add */0 :
          return add_todos(command._0);
      case /* Delete */1 :
          return delete_todo(command._0);
      case /* Done */2 :
          return completed(command._0);
      
    }
  }
}

function value_from_option(x) {
  if (x !== undefined) {
    return x;
  } else {
    return -1;
  }
}

function get_type(argument, argv) {
  if (argument === "add") {
    if (argv.length > 3) {
      return {
              TAG: /* Add */0,
              _0: Caml_array.get(argv, 3)
            };
    } else {
      console.log("Error: Missing todo string. Nothing added!");
      return /* Empty */3;
    }
  }
  if (argument === "del") {
    if (argv.length > 3) {
      var index = Caml_array.get(argv, 3);
      var x = Belt_Int.fromString(index);
      var value = x !== undefined ? x : -1;
      return {
              TAG: /* Delete */1,
              _0: value
            };
    }
    console.log("Error: Missing NUMBER for deleting todo.");
    return /* Empty */3;
  }
  if (argument !== "done") {
    if (argument === "ls") {
      return /* List */2;
    } else if (argument === "report") {
      return /* Report */1;
    } else {
      return /* Help */0;
    }
  }
  if (argv.length > 3) {
    var index$1 = Caml_array.get(argv, 3);
    var x$1 = Belt_Int.fromString(index$1);
    var value$1 = x$1 !== undefined ? x$1 : -1;
    return {
            TAG: /* Done */2,
            _0: value$1
          };
  }
  console.log("Error: Missing NUMBER for marking todo as done.");
  return /* Empty */3;
}

var argv = process.argv;

if (argv.length < 3) {
  console.log(usage);
} else {
  var command = Caml_array.get(argv, 2);
  exe_command(get_type(command, argv));
}

exports.getToday = getToday;
exports.encoding = encoding;
exports.pending_todos_file = pending_todos_file;
exports.completed_todos_file = completed_todos_file;
exports.read_file = read_file;
exports.append_file = append_file;
exports.write_file = write_file;
exports.list_of_todos = list_of_todos;
exports.add_todos = add_todos;
exports.usage = usage;
exports.help = help;
exports.completed = completed;
exports.delete_todo = delete_todo;
exports.report = report;
exports.exe_command = exe_command;
exports.value_from_option = value_from_option;
exports.get_type = get_type;
exports.argv = argv;
/*  Not a pure module */
