import { combineReducers } from 'redux';

import command, { CommandState } from './command';
import compile, { CompileState } from './compile';
import deploy, { DeployState } from './deploy';
import { DryRunState, dryRun } from './dry-run';
import editor, { EditorState } from './editor';
import evaluateFunction, { EvaluateFunctionState } from './evaluate-function';
import evaluateValue, { EvaluateValueState } from './evaluate-value';
import examples, { ExamplesState } from './examples';
import generateDeployScript, {
  GenerateDeployScriptState,
} from './generate-deploy-script';
import loading, { LoadingState } from './loading';
import result, { ResultState } from './result';
import share, { ShareState } from './share';
import version, { VersionState } from './version';

export interface AppState {
  version: VersionState;
  editor: EditorState;
  share: ShareState;
  compile: CompileState;
  dryRun: DryRunState;
  deploy: DeployState;
  evaluateFunction: EvaluateFunctionState;
  evaluateValue: EvaluateValueState;
  generateDeployScript: GenerateDeployScriptState;
  result: ResultState;
  command: CommandState;
  examples: ExamplesState;
  loading: LoadingState;
}

const reducer = combineReducers({
  editor,
  share,
  compile,
  dryRun,
  deploy,
  evaluateFunction,
  evaluateValue,
  generateDeployScript,
  result,
  command,
  examples,
  loading,
  version,
});

export default reducer;
