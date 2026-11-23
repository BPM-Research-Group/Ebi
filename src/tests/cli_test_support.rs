use crate::ebi_framework::ebi_command::EBI_COMMANDS;

/// Checks that a command line is accepted by the command line interface, and
/// that the inputs on it are read.
///
/// This covers the stage between clap and the command: the names of the
/// command and its groups (long or short), the positional arguments, and the
/// reading of the input files. It does not run the command, and does not
/// itself change the arithmetic mode. A test elsewhere that does change it
/// could still affect how a fraction is read here.
///
/// `command` is the names of the groups and the command, without the program
/// name. `inputs` is the positional arguments, one per input of the command.
pub(crate) fn assert_cli_reads_inputs(command: &[&str], inputs: &[&str]) {
    let line = ["ebi"]
        .into_iter()
        .chain(command.iter().copied())
        .chain(inputs.iter().copied());
    let line_text = line.clone().collect::<Vec<_>>().join(" ");

    let matches = EBI_COMMANDS
        .build_cli()
        .try_get_matches_from(line)
        .unwrap_or_else(|e| panic!("`{line_text}` was not accepted:\n{e}"));

    //walk down to the command, collecting the (long) names clap resolved
    let mut names = vec![];
    let mut leaf = &matches;
    while let Some((name, sub_matches)) = leaf.subcommand() {
        names.push(name);
        leaf = sub_matches;
    }

    let root = EBI_COMMANDS.long_name();
    let path = EBI_COMMANDS
        .find_command_with_string(&format!("{root} {}", names.join(" ")))
        .unwrap_or_else(|| panic!("`{line_text}` resolved to no command"));
    let read = path
        .last()
        .unwrap()
        .read_inputs(leaf)
        .unwrap_or_else(|e| {
            panic!("`{line_text}` inputs were not read: {e:?}")
        });

    assert_eq!(read.len(), inputs.len(), "`{line_text}`");
}
