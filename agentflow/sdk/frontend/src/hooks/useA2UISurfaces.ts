import { useCallback, useState } from 'react';

import type {
  A2UIClearEvent,
  A2UIComponentEvent,
  A2UIComponentNode,
  A2UIUpdateEvent,
} from '../types/events';

export type A2UIEvent = A2UIComponentEvent | A2UIUpdateEvent | A2UIClearEvent;
export type A2UISurfaceMap = Record<string, A2UIComponentNode[]>;

function replaceComponent(
  components: A2UIComponentNode[],
  replacement: A2UIComponentNode,
): A2UIComponentNode[] {
  if (!replacement.id) {
    return [...components, replacement];
  }
  const index = components.findIndex((component) => component.id === replacement.id);
  if (index === -1) {
    return [...components, replacement];
  }
  return components.map((component, componentIndex) =>
    componentIndex === index ? replacement : component
  );
}

export function useA2UISurfaces(initialState: A2UISurfaceMap = {}): {
  surfaces: A2UISurfaceMap;
  applyEvent: (event: A2UIEvent) => void;
  clearSurface: (surfaceId: string) => void;
  reset: () => void;
} {
  const [surfaces, setSurfaces] = useState<A2UISurfaceMap>(initialState);

  const applyEvent = useCallback((event: A2UIEvent) => {
    setSurfaces((current) => {
      if (event.event_type === 'a2ui.clear') {
        return {
          ...current,
          [event.surface_id]: [],
        };
      }

      if (event.event_type === 'a2ui.component') {
        return {
          ...current,
          [event.surface_id]: replaceComponent(current[event.surface_id] || [], event.component),
        };
      }

      const replacement = event.updates.component;
      if (typeof replacement !== 'object' || replacement === null || Array.isArray(replacement)) {
        return current;
      }

      return {
        ...current,
        [event.surface_id]: replaceComponent(
          current[event.surface_id] || [],
          replacement as A2UIComponentNode,
        ),
      };
    });
  }, []);

  const clearSurface = useCallback((surfaceId: string) => {
    setSurfaces((current) => ({
      ...current,
      [surfaceId]: [],
    }));
  }, []);

  const reset = useCallback(() => {
    setSurfaces(initialState);
  }, [initialState]);

  return {
    surfaces,
    applyEvent,
    clearSurface,
    reset,
  };
}
