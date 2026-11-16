
      (keys == ["auth","initial","steps"]) and
      ((.steps | length) > 0) and
      ([.steps[] | keys] | all(. == ["accepted","change","event","signer","state"]))
    