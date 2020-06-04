import { ExampleState } from './example';

export enum ActionType {
  ChangeSelected = 'examples-change-selected',
  ClearSelected = 'examples-clear-selected'
}

export interface ExampleItem {
  id: string;
  name: string;
}

export interface ExamplesState {
  selected: ExampleState | null;
  list: ExampleItem[];
}

export class ChangeSelectedAction {
  public readonly type = ActionType.ChangeSelected;
  constructor(public payload: ExamplesState['selected']) { }
}

export class ClearSelectedAction {
  public readonly type = ActionType.ClearSelected;
}

type Action = ChangeSelectedAction | ClearSelectedAction;

export const DEFAULT_STATE: ExamplesState = {
  selected: null,
  list: []
};


if (process.env.NODE_ENV === 'development') {

  // The name value configured in this list will only be for the development environment.
  // For other environments, the name value will be taken directly from your contract's yaml configuration.
  DEFAULT_STATE.list = [
    { id: 'FEb62HL7onjg1424eUsGSg', name: 'Increment (PascaLIGO)' },
    { id: 'MzkMQ1oiVHJqbcfUuVFKTw', name: 'Increment (CameLIGO)' },
    { id: 'JPhSOehj_2MFwRIlml0ymQ', name: 'Increment (ReasonLIGO)' },
    { id: 'yP-THvmURsaqHxpwCravWg', name: 'ID (PascaLIGO)' },
    { id: 'ehDv-Xaf70mQoiPhQDTAUQ', name: 'ID (CameLIGO)' },
    { id: 'CpnK7TFuUjJiQTT8KiiGyQ', name: 'ID (ReasonLIGO)' },
    { id: 'NCo8yadjxAZbW5QlojmA0w', name: 'Hashlock (PascaLIGO)' },
    { id: 'v1A26q31HZj0RADecjNg3A', name: 'Hashlock (CameLIGO)' },
    { id: 'D0EjGZZWuK2ILzPqtUDrQg', name: 'Hashlock (ReasonLIGO)' },
    { id: 'QJEg0kbU3mFVI6IRgweI5Q', name: 'FA1.2 (PascaLIGO)' },
    { id: 'ZKLbvE7Xp8Rta-fPrTFJww', name: 'FA1.2 - revised (PascaLIGO)' },
    { id: 'ZB_MHTmkjoFqSvFfGOnR7Q', name: 'FA1.2 (CameLIGO)' },
    { id: 's9rWty_jRYBQwwZwebOAQg', name: 'FA1.2 (ReasonLIGO)' }
  ];
}

export default (state = DEFAULT_STATE, action: Action): ExamplesState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        selected: action.payload
      };
    case ActionType.ClearSelected:
      return {
        ...state,
        selected: null
      };
    default:
      return state;
  }
};
