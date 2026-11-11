
      (keys == ["auth","traces","view"]) and
      ((.traces | length) > 0) and
      ([.traces[] | keys] | all(. == ["initial","schema","steps","version"]))
    