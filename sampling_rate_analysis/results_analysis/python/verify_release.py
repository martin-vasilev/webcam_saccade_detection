"""Independent stdlib verification of packaged aggregates/presentation, not raw-data recomputation."""
import argparse,csv,hashlib,json,math,pathlib,sys

def rows(p):
    with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def number(s):
    try:return float(s)
    except (TypeError,ValueError):return math.nan
def near(a,b):return (math.isnan(a) and math.isnan(b)) or math.isclose(a,b,abs_tol=1e-11,rel_tol=1e-11)
def verify(root, strict_render_bytes=False):
    render_differences=[]
    checks={'frozen_hashes':0,'classification_rows':0,'transport_identities':0,'review_values':0,'curve_points':0,'part1_plot_values':0}
    for r in rows(root/'config/frozen_manifest.csv'):
        assert sha(root/r['path'])==r['sha256'],r['path']
        checks['frozen_hashes']+=1
    code=json.loads((root/'config/source_code_manifest.json').read_text())
    for r in code:assert sha(root/r['path'])==r['release_sha256'],r['path']
    checks['archived_code_hashes']=len(code)
    science=root/'config/executable_code_manifest.json'
    if science.exists():
        entries=json.loads(science.read_text())
        for r in entries:assert sha(root/r['path'])==r['release_sha256'],r['path']
        checks['executable_scientific_code_hashes']=len(entries)
    inventory=root/'validation/release_inventory.csv'
    if inventory.exists():
        for r in rows(inventory):
            actual=sha(root/r['path'])
            is_render=r['path'].startswith('output/') and pathlib.Path(r['path']).suffix.lower() in {'.png','.pdf'}
            if actual!=r['sha256'] and is_render:
                render_differences.append({'path':r['path'],'snapshot_sha256':r['sha256'],'actual_sha256':actual})
                if strict_render_bytes:raise AssertionError(('strict render inventory',r['path']))
            else:assert actual==r['sha256'],('release inventory',r['path'])
    for f in (root/'data/frozen').rglob('*.csv'):
        for r in rows(f):
            if all(c in r for c in ['TP','TN','FP','FN','MCC']):
                tp,tn,fp,fn=[number(r[c]) for c in ['TP','TN','FP','FN']]
                den=(tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
                metric={'MCC':(tp*tn-fp*fn)/math.sqrt(den) if den else math.nan,
                        'F1':2*tp/(2*tp+fp+fn) if 2*tp+fp+fn else math.nan,
                        'precision':tp/(tp+fp) if tp+fp else math.nan,
                        'recall':tp/(tp+fn) if tp+fn else math.nan}
                for k,v in metric.items():
                    if k in r:assert near(v,number(r[k])),(f.name,k)
                for k in ['scored_rows','evaluation_rows']:
                    if k in r:assert tp+tn+fp+fn==number(r[k]),(f.name,k)
                checks['classification_rows']+=1
            for k in ['multi','N','Mean','SD']:
                if 'distance_'+k in r:
                    d,s=number(r['distance_'+k]),number(r['similarity_'+k])
                    assert near(1/(1+d),s),(f.name,k)
                    checks['transport_identities']+=1
    # Every review cell must be an unchanged source field in its corresponding source row.
    mapping={
      'available_configurations_and_support':'part1/configurations_and_support.csv',
      'sample_level_heldout_policy_and_CNN_Raw':None,
      'sample_level_prediction_availability':None,
      'development_selected_fixed_configurations':'part2b/final_fixed_configurations.csv',
      'fixed_configuration_stability':'part2b/configuration_stability.csv',
      'event_transport_common_complete':'part3/wasserstein_harmonised.csv',
      'native_events_all_available_trials':'part3/event_distribution_summary.csv',
      'saccade_velocity_harmonised':'part3/saccade_velocity_summary.csv'}
    for name,src in mapping.items():
        f=root/'output/review/tables'/f'{name}.csv'
        assert f.exists(),name
        candidates=rows(root/'data/frozen'/src) if src else rows(root/'data/frozen/part2/heldout_performance.csv')+rows(root/'data/frozen/part2/cnn_five_branch_performance.csv')
        for r in rows(f):
            def matches(a):
                for k,v in r.items():
                    if k not in a:return False
                    x,y=number(v),number(a[k])
                    if not (near(x,y) if not math.isnan(x) or not math.isnan(y) else v==a[k]):return False
                return True
            assert any(matches(a) for a in candidates),(name,r)
            checks['review_values']+=len(r)
    # Confirm primary table policy, row counts, and each scoring-identity denominator.
    a=rows(root/'output/review/tables/sample_level_heldout_policy_and_CNN_Raw.csv')
    assert len(a)==20
    assert {r['evaluation_support'] for r in a}=={'own_policy_paired_joint','frozen_CNN_common'}
    w=rows(root/'data/frozen/part3/wasserstein_harmonised.csv')
    for event,n in [('fixation',516),('saccade',366)]:
        z=[r for r in w if r['support_policy']=='primary_five_common_complete' and r['reference_definition']=='historical' and r['duration_variant']=='native' and r['event']==event]
        assert len(z)==10 and {int(r['n_trials']) for r in z}=={n} and len({r['support_sha256'] for r in z})==1
    v=rows(root/'data/frozen/part3/saccade_velocity_summary.csv')
    missing=rows(root/'data/frozen/part3/saccade_velocity_missingness.csv')
    for r in v:
        assert int(r['events'])==int(r['defined_events'])+int(r['undefined_events'])
        z=[s for s in missing if all(s[k]==r[k] for k in ['model','representation','reference_definition','velocity_rule'])]
        assert sum(int(s['events']) for s in z)==int(r['events'])
        assert sum(int(s['events']) for s in z if s['reason']=='defined')==int(r['defined_events'])
    for r in rows(root/'data/frozen/part3/event_distribution_summary.csv'):
        assert int(r['available_trials'])+int(r['unavailable_trials'])==int(r['trials'])
        assert near(number(r['trial_N_mean'])*number(r['N_denominator']),number(r['pooled_events']))
        assert int(r['SD_denominator'])<=int(r['Mean_denominator'])<=int(r['N_denominator'])
    checks['event_summary_rows']=88;checks['velocity_summary_rows']=len(v)
    curves=rows(root/'data/frozen/part3/figure_source_curves.csv')
    assert len(curves)==84050 and len({r['figure'] for r in curves})==12
    checks['curve_points']=len(curves)
    ledger=root/'output/supplementary/part1_presentation/validation/plot_ledger.csv'
    names={'grid':'grid','hmm':'hmm','cnn':'cnn','common':'common','selected_context':'selected_context'}
    for r in rows(ledger):
        tab=rows(root/f"data/frozen/part1/{names[r['source_table']]}_source.csv")
        src=tab[int(r['source_row'])-1]
        assert near(number(r['MCC']),number(src['MCC'])) and number(r['scored_rows'])==number(src['scored_rows'])
        checks['part1_plot_values']+=1
    result={'passed':True,'scope':'Independent archived aggregate identities and review-source values; not event reconstruction, selection, transport re-solving or model training',**checks,'render_hash_differences':render_differences,'render_review_required':bool(render_differences),'strict_render_bytes':strict_render_bytes,'python':sys.version.split()[0]}
    (root/'validation/independent_release.json').write_text(json.dumps(result,indent=2)+'\n',encoding='utf-8')
    print(json.dumps(result,indent=2))
if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('--root',type=pathlib.Path,default=pathlib.Path(__file__).resolve().parents[1]);parser.add_argument('--strict-render-bytes',action='store_true',help='Also require snapshot byte identity of PDF/PNG renders; default validates science and reports render differences for separate review');a=parser.parse_args();verify(a.root.resolve(),a.strict_render_bytes)
