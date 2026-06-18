{
  "executions": [
    {
      "activity": "a",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "a",
        "org:resource": "Blue",
        "time:timestamp": "2017-01-03 00:00:00 +01:00"
      },
      "fired_transition": 0,
      "move_index": 0,
      "move_index_of_enablement": null,
      "other_enabled_transitions": [
        3
      ],
      "other_enabled_transitions_resource_utilisation": [
        "0"
      ],
      "resource": "Blue",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-03 00:00:00 +01:00",
      "trace": 0
    },
    {
      "activity": "c",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "c",
        "org:resource": "Red",
        "time:timestamp": "2017-01-04 00:00:00 +01:00"
      },
      "fired_transition": 3,
      "move_index": 0,
      "move_index_of_enablement": null,
      "other_enabled_transitions": [
        0
      ],
      "other_enabled_transitions_resource_utilisation": [
        "0"
      ],
      "resource": "Red",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-04 00:00:00 +01:00",
      "trace": 2
    },
    {
      "activity": "a",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "a",
        "org:resource": "Blue",
        "time:timestamp": "2017-01-06 00:00:00 +01:00"
      },
      "fired_transition": 0,
      "move_index": 0,
      "move_index_of_enablement": null,
      "other_enabled_transitions": [
        3
      ],
      "other_enabled_transitions_resource_utilisation": [
        "1"
      ],
      "resource": "Blue",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-06 00:00:00 +01:00",
      "trace": 1
    },
    {
      "activity": "c",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "c",
        "org:resource": "Red",
        "time:timestamp": "2017-01-09 00:00:00 +01:00"
      },
      "fired_transition": 3,
      "move_index": 1,
      "move_index_of_enablement": null,
      "other_enabled_transitions": [
        1,
        2
      ],
      "other_enabled_transitions_resource_utilisation": [
        "1/2",
        "0"
      ],
      "resource": "Red",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-09 00:00:00 +01:00",
      "trace": 0
    },
    {
      "activity": "e",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "e",
        "org:resource": "Blue",
        "time:timestamp": "2017-01-10 00:00:00 +01:00"
      },
      "fired_transition": 4,
      "move_index": 1,
      "move_index_of_enablement": 0,
      "other_enabled_transitions": [
        0
      ],
      "other_enabled_transitions_resource_utilisation": [
        "0"
      ],
      "resource": "Blue",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-10 00:00:00 +01:00",
      "trace": 2
    },
    {
      "activity": "a",
      "also_in_log": false,
      "event_attributes": null,
      "fired_transition": 0,
      "move_index": 2,
      "move_index_of_enablement": null,
      "other_enabled_transitions": [],
      "other_enabled_transitions_resource_utilisation": [],
      "resource": null,
      "resource_utilisation_fired_transition": null,
      "time_of_execution": null,
      "trace": 2
    },
    {
      "activity": "b",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "b",
        "org:resource": "Red",
        "time:timestamp": "2017-01-15 00:00:00 +01:00"
      },
      "fired_transition": 2,
      "move_index": 2,
      "move_index_of_enablement": 0,
      "other_enabled_transitions": [
        1,
        4
      ],
      "other_enabled_transitions_resource_utilisation": [
        "1/2",
        "1"
      ],
      "resource": "Red",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-15 00:00:00 +01:00",
      "trace": 0
    },
    {
      "activity": "e",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "e",
        "org:resource": "Blue",
        "time:timestamp": "2017-01-16 00:00:00 +01:00"
      },
      "fired_transition": 4,
      "move_index": 3,
      "move_index_of_enablement": 1,
      "other_enabled_transitions": [],
      "other_enabled_transitions_resource_utilisation": [],
      "resource": "Blue",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-16 00:00:00 +01:00",
      "trace": 0
    },
    {
      "activity": "d",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "d",
        "org:resource": "Blue",
        "time:timestamp": "2017-01-18 00:00:00 +01:00"
      },
      "fired_transition": 1,
      "move_index": 1,
      "move_index_of_enablement": 0,
      "other_enabled_transitions": [
        2,
        3
      ],
      "other_enabled_transitions_resource_utilisation": [
        "1",
        "1"
      ],
      "resource": "Blue",
      "resource_utilisation_fired_transition": "1/2",
      "time_of_execution": "2017-01-18 00:00:00 +01:00",
      "trace": 1
    },
    {
      "activity": "d",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "d",
        "org:resource": "Red",
        "time:timestamp": "2017-01-19 00:00:00 +01:00"
      },
      "fired_transition": 1,
      "move_index": 3,
      "move_index_of_enablement": 2,
      "other_enabled_transitions": [
        2
      ],
      "other_enabled_transitions_resource_utilisation": [
        "0"
      ],
      "resource": "Red",
      "resource_utilisation_fired_transition": "1/2",
      "time_of_execution": "2017-01-19 00:00:00 +01:00",
      "trace": 2
    },
    {
      "activity": "c",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "c",
        "org:resource": "Red",
        "time:timestamp": "2017-01-21 00:00:00 +01:00"
      },
      "fired_transition": 3,
      "move_index": 2,
      "move_index_of_enablement": null,
      "other_enabled_transitions": [],
      "other_enabled_transitions_resource_utilisation": [],
      "resource": "Red",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-21 00:00:00 +01:00",
      "trace": 1
    },
    {
      "activity": "e",
      "also_in_log": true,
      "event_attributes": {
        "concept:name": "e",
        "org:resource": "Blue",
        "time:timestamp": "2017-01-24 00:00:00 +01:00"
      },
      "fired_transition": 4,
      "move_index": 4,
      "move_index_of_enablement": 2,
      "other_enabled_transitions": [],
      "other_enabled_transitions_resource_utilisation": [],
      "resource": "Blue",
      "resource_utilisation_fired_transition": "0",
      "time_of_execution": "2017-01-24 00:00:00 +01:00",
      "trace": 1
    }
  ]
}