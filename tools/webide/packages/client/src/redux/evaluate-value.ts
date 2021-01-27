import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum ActionType {
  ChangeEntrypoint = 'evaluate-value-change-entrypoint'
}

export interface EvaluateValueState {
  entrypoint: string;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: EvaluateValueState['entrypoint']) {}
}

type Action = ChangeEntrypointAction | ChangeSelectedExampleAction;

const DEFAULT_STATE: EvaluateValueState = {
  entrypoint: ''
};

const EvaluateValue = (state = DEFAULT_STATE, action: Action): EvaluateValueState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.evaluateValue)
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
    default:
      return state;
  }
};

export default EvaluateValue;