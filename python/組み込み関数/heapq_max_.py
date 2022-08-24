import heapq


def heappush_max(heap, item):
    """Push item onto heap, maintaining the heap invariant."""
    heap.append(item)
    heapq._siftdown_max(heap, 0, len(heap) - 1)


def heappop_max(heap):
    return heapq._heappop_max(heap)


def heapreplace_max(heap, item):
    heapq._heapreplace_max(heap, item)


def heappushpop_max(heap, item):
    """Fast version of a heappush followed by a heappop."""
    if heap and heap[0] > item:
        item, heap[0] = heap[0], item
        heapq._siftup_max(heap, 0)
    return item


def heapify_max(x):
    heapq._heapify_max(x)


def _siftup_max(heap, pos):
    heapq._siftup_max(heap, pos)


def _siftdown_max(heap, startpos, pos):
    heapq._siftdown_max(heap, startpos, pos)
