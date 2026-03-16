import type { ReactNode } from 'react';

import type { A2UIComponentNode } from '../types/events';

export interface A2UISurfaceProps {
  components: A2UIComponentNode[];
}

function renderComponent(component: A2UIComponentNode, keyPrefix: string): ReactNode {
  const componentKey = component.id || keyPrefix;
  const props = component.props || {};
  const children = component.children || [];

  if (component.type === 'text') {
    const content =
      typeof props.content === 'string'
        ? props.content
        : typeof props.text === 'string'
          ? props.text
          : '';
    return <pre key={componentKey}>{content}</pre>;
  }

  if (component.type === 'card') {
    return (
      <article key={componentKey}>
        <h3>{typeof props.title === 'string' ? props.title : 'Card'}</h3>
        {children.map((child, index) => renderComponent(child, `${componentKey}-${index}`))}
      </article>
    );
  }

  if (component.type === 'list') {
    return (
      <ul key={componentKey}>
        {children.map((child, index) => (
          <li key={`${componentKey}-${index}`}>
            {renderComponent(child, `${componentKey}-${index}`)}
          </li>
        ))}
      </ul>
    );
  }

  return <pre key={componentKey}>{JSON.stringify(component, null, 2)}</pre>;
}

export function A2UISurface({ components }: A2UISurfaceProps) {
  return <>{components.map((component, index) => renderComponent(component, `${index}`))}</>;
}
