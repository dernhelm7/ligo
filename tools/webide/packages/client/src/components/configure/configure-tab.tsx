import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled, { css } from 'styled-components';

import { CompileAction } from '../../redux/actions/compile';
import { DeployAction } from '../../redux/actions/deploy';
import { DryRunAction } from '../../redux/actions/dry-run';
import { EvaluateFunctionAction } from '../../redux/actions/evaluate-function';
import { EvaluateValueAction } from '../../redux/actions/evaluate-value';
import { GenerateDeployScriptAction } from '../../redux/actions/generate-deploy-script';
import { AppState } from '../../redux/app';
import { ChangeDispatchedAction, ChangeSelectedAction, CommandState } from '../../redux/command';
import { Command } from '../../redux/types';
import { Option, Select } from '../form/select';
import { CompilePaneComponent } from './compile-pane';
import { DeployPaneComponent } from './deploy-pane';
import { DryRunPaneComponent } from './dry-run-pane';
import { EvaluateFunctionPaneComponent } from './evaluate-function-pane';
import { EvaluateValuePaneComponent } from './evaluate-value-pane';
import { GenerateDeployScriptPane } from './generate-deploy-script-pane';

const Container = styled.div<{ visible?: boolean }>`
  position: absolute;
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  padding: 1em 1em 0 1em;

  display: flex;
  flex-direction: column;

  transform: translateX(-100%);
  transition: transform 0.2s ease-in;

  ${props =>
    props.visible ?
    css`
      transform: translateX(0px);
    ` : css`
      visibility: hidden;
    `} 
`;

const CommonActionsGroup = styled.div`
  display: flex;
  align-items: center;
`;

const RunButton = styled.div`
  cursor: pointer;
  user-select: none;

  display: flex;
  justify-content: center;
  align-items: center;
  flex: 1;
  min-height: 2em;
  min-width: 3em;
  margin-left: 1em;

  color: white;
  background-color: var(--orange);

  &:hover {
    box-shadow: 1px 3px 10px 0px rgba(153, 153, 153, 0.4);
  }
`;

const SelectCommand = styled(Select)`
  flex: 2;

  &:hover {
    box-shadow: 1px 3px 10px 0px rgba(153, 153, 153, 0.4);
  }
`;

function createAction(command: Command) {
  switch (command) {
    case Command.Compile:
      return new CompileAction();
    case Command.DryRun:
      return new DryRunAction();
    case Command.Deploy:
      return new DeployAction();
    case Command.EvaluateValue:
      return new EvaluateValueAction();
    case Command.EvaluateFunction:
      return new EvaluateFunctionAction();
    case Command.GenerateDeployScript:
      return new GenerateDeployScriptAction();
    default:
      throw new Error('Unsupported command');
  }
}

export const ConfigureTabComponent = (props: {
  selected?: boolean;
  onRun?: () => void;
}) => {
  const dispatchedAction = useSelector<
    AppState,
    CommandState['dispatchedAction']
  >(state => state.command.dispatchedAction);

  const command = useSelector<AppState, CommandState['selected']>(
    state => state.command.selected
  );

  const dispatch = useDispatch();

  return (
    <Container visible={props.selected}>
      <CommonActionsGroup>
        <SelectCommand
          id="command-select"
          value={command}
          onChange={command => {
            dispatch({ ...new ChangeSelectedAction(command) });
          }}
        >
          <Option value={Command.Compile}>Compile</Option>
          <Option value={Command.Deploy}>Deploy</Option>
          <Option value={Command.DryRun}>Dry Run</Option>
          <Option value={Command.EvaluateFunction}>Evaluate Function</Option>
          <Option value={Command.EvaluateValue}>Evaluate Value</Option>
          <Option value={Command.GenerateDeployScript}>Generate Deploy Script</Option>
        </SelectCommand>
        <RunButton
          id="run"
          onClick={() => {
            if (dispatchedAction) {
              dispatchedAction.cancel();
            }

            const newAction = createAction(command);
            dispatch(newAction.getAction());
            dispatch({ ...new ChangeDispatchedAction(newAction) });

            props.onRun!();
          }}
        >
          Run
        </RunButton>
      </CommonActionsGroup>
      {(command === Command.Compile && (
        <CompilePaneComponent></CompilePaneComponent>
      )) ||
        (command === Command.DryRun && (
          <DryRunPaneComponent></DryRunPaneComponent>
        )) ||
        (command === Command.Deploy && (
          <DeployPaneComponent></DeployPaneComponent>
        )) ||
        (command === Command.EvaluateFunction && (
          <EvaluateFunctionPaneComponent></EvaluateFunctionPaneComponent>
        )) ||
        (command === Command.EvaluateValue && (
          <EvaluateValuePaneComponent></EvaluateValuePaneComponent>
        )) ||
        (command === Command.GenerateDeployScript && (
          <GenerateDeployScriptPane></GenerateDeployScriptPane>
        ))}
    </Container>
  );
};
